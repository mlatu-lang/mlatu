{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Mlatu.Tokenize
-- Description : Lexing
-- Copyright   : (c) Caden Haustin, 2021
-- License     : MIT
-- Maintainer  : mlatu@brightlysalty.33mail.com
-- Stability   : experimental
-- Portability : GHC
module Mlatu.Tokenize
  ( tokenize,
  )
where

import Data.ByteString qualified as BS
import Data.Char (isLetter, isPunctuation, isSymbol)
import Data.Text qualified as Text
import Mlatu.Base (Base (..))
import Mlatu.Bits
  ( FloatBits (..),
    IntegerBits (..),
  )
import Mlatu.Indent (Indent (..))
import Mlatu.Informer (Informer (..))
import Mlatu.Layoutness (Layoutness (..))
import Mlatu.Literal (FloatLiteral (FloatLiteral), IntegerLiteral (IntegerLiteral))
import Mlatu.Located (Located (..))
import Mlatu.Name (Unqualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Report qualified as Report
import Mlatu.Token (Token (..))
import Numeric (readHex, readOct)
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Parsec (Column, ParsecT, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Text.PrettyPrint qualified as Pretty

-- | Lexes a source fragment into a list of tokens, annotated with their source
-- locations and indent levels.
tokenize ::
  (Informer m) =>
  -- | Initial source line.
  Int ->
  -- | Source file path.
  FilePath ->
  -- | Source text.
  Text ->
  -- | Lexed tokens.
  m [Located (Token 'Layout)]
tokenize line path txt = case Parsec.runParser
  (setPos *> fileTokenizer)
  1
  path
  txt of
  Left parseError -> do
    report $ Report.parseError parseError
    halt
  Right result -> pure result
  where
    setPos = Parsec.setPosition $ Parsec.newPos path line 1

type Tokenizer a = ParsecT Text Column Identity a

fileTokenizer :: Tokenizer [Located (Token 'Layout)]
fileTokenizer = silenceTokenizer *> tokensTokenizer <* Parsec.eof

silenceTokenizer :: Tokenizer ()
silenceTokenizer = Parsec.skipMany (comment <|> whitespace)
  where
    whitespace = Parsec.skipMany1 (newline <|> nonNewline) <?> "whitespace"

    newline = do
      void (Parsec.endOfLine *> many nonNewline)
      pos <- Parsec.getPosition
      Parsec.putState (Parsec.sourceColumn pos)

    nonNewline = void (Parsec.satisfy (`elem` ("\r " :: String)))

    comment = single <|> multi <?> "comment"

    single =
      Parsec.try (Parsec.string "//")
        *> (Parsec.anyChar `skipManyTill` (newline <|> Parsec.eof))

    multi = void (Parsec.between start end contents)
      where
        contents = characters *> optional multi <* characters
        characters =
          Parsec.skipMany $
            Parsec.notFollowedBy (start <|> end) *> Parsec.anyChar
        start = Parsec.try (Parsec.string "/*")
        end = Parsec.string "*/"

    skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
    a `skipManyTill` b = void (Parsec.try b) <|> a *> (a `skipManyTill` b)

tokensTokenizer :: Tokenizer [Located (Token 'Layout)]
tokensTokenizer = tokenTokenizer `Parsec.sepEndBy` silenceTokenizer

rangedTokenizer :: Tokenizer (Token 'Layout) -> Tokenizer (Located (Token 'Layout))
rangedTokenizer parser = do
  column <- Parsec.getState
  begin <- Parsec.getPosition
  result <- parser
  end <- Parsec.getPosition
  return $ At (Origin.range begin end) (Indent column) result

blockBegin :: Tokenizer (Token l)
blockBegin = BlockBegin <$ Parsec.char '{'

blockEnd :: Tokenizer (Token l)
blockEnd = BlockEnd <$ Parsec.char '}'

singleQuote :: Tokenizer Char
singleQuote = Parsec.char '\''

characterLiteral :: Tokenizer (Token l)
characterLiteral = do
  mc <- Parsec.between singleQuote singleQuote $ character '\''
  case mc of
    Just c -> return (Character c)
    Nothing -> Parsec.unexpected "empty character literal"

bracketOperator :: Char -> Tokenizer (Token 'Layout)
bracketOperator char =
  Operator (Unqualified (one char))
    <$ Parsec.try (Parsec.char char <* Parsec.notFollowedBy symbol)

character :: Char -> Tokenizer (Maybe Char)
character quote = char <|> escape
  where
    char = (<?> "character") $ do
      c <- Parsec.noneOf ('\\' : [quote])
      case c of
        '\n' ->
          Parsec.unexpected
            "newline in text literal; use an escape, gap, or paragraph instead"
            <?> "character or escape"
        _ -> return $ Just c

escape :: Tokenizer (Maybe Char)
escape =
  (<?> "escape") $
    Parsec.char '\\'
      *> Parsec.choice
        [ Just <$> Parsec.oneOf "\\\"'",
          Just '\a' <$ Parsec.char 'a',
          Just '\b' <$ Parsec.char 'b',
          Just '\f' <$ Parsec.char 'f',
          Just '\n' <$ Parsec.char 'n',
          Just '\r' <$ Parsec.char 'r',
          Just '\t' <$ Parsec.char 't',
          Just '\v' <$ Parsec.char 'v',
          Just <$> (Parsec.space <* Parsec.spaces),
          Nothing <$ Parsec.char '&'
        ]

symbol :: Tokenizer Char
symbol =
  Parsec.notFollowedBy special
    *> Parsec.choice (map Parsec.satisfy [isSymbol, isPunctuation])

special :: Tokenizer Char
special = Parsec.oneOf "\"'(),:[\\]_{}"

comma :: Tokenizer (Token l)
comma = Comma <$ Parsec.char ','

tokenTokenizer :: Tokenizer (Located (Token 'Layout))
tokenTokenizer =
  rangedTokenizer $
    Parsec.choice
      [ blockBegin,
        blockEnd,
        characterLiteral,
        do
          mc <-
            Parsec.between (Parsec.char '\x2018') (Parsec.char '\x2019') $
              nestableCharacter '\x2018' '\x2019'
          case mc of
            [c] -> return (Character c)
            [] -> Parsec.unexpected "empty character literal"
            _multiLit -> Parsec.unexpected "multi-character literal",
        comma,
        Parsec.try $
          Ellipsis
            <$ Parsec.choice (map Parsec.string ["...", "\x2026"]),
        GroupBegin <$ Parsec.char '(',
        GroupEnd <$ Parsec.char ')',
        Parsec.try $ Ignore <$ Parsec.char '_' <* Parsec.notFollowedBy letter,
        Parsec.try $
          VocabLookup
            <$ Parsec.choice (map Parsec.string ["::", "\x2237"]),
        Colon <$ Parsec.char ':',
        VectorBegin <$ Parsec.char '[',
        VectorEnd <$ Parsec.char ']',
        Reference <$ Parsec.char '\\',
        Text <$> paragraph,
        Text
          <$> Parsec.between
            (Parsec.char '"')
            (Parsec.char '"' <?> "closing double quote")
            text,
        Text
          <$> Parsec.between
            (Parsec.char '\x201C')
            (Parsec.char '\x201D' <?> "closing right double quote")
            (nestableText '\x201C' '\x201D'),
        Parsec.try $ do
          sign <- Parsec.optionMaybe (Parsec.oneOf "+-\x2212")
          let applySign :: (Num a) => Maybe Char -> a -> a
              applySign s = if s `elem` [Just '-', Just '\x2212'] then negate else id
              base ::
                (Num a) =>
                Char ->
                String ->
                (String -> a) ->
                Base ->
                String ->
                Tokenizer (Base, a)
              base prefix digits readBase hint desc =
                (,) hint . readBase
                  <$> ( Parsec.char prefix
                          *> Parsec.many1 (Parsec.oneOf digits <?> (desc ++ " digit"))
                      )
              integerBits =
                Parsec.option Signed32 $
                  Parsec.choice
                    [ Parsec.char 'i'
                        *> Parsec.choice
                          [ Signed8 <$ Parsec.string "8",
                            Signed16 <$ Parsec.string "16",
                            Signed32 <$ Parsec.string "32",
                            Signed64 <$ Parsec.string "64"
                          ],
                      Parsec.char 'u'
                        *> Parsec.choice
                          [ Unsigned8 <$ Parsec.string "8",
                            Unsigned16 <$ Parsec.string "16",
                            Unsigned32 <$ Parsec.string "32",
                            Unsigned64 <$ Parsec.string "64"
                          ]
                    ]
          Parsec.choice
            [ Parsec.try $ do
                (hint, value) <-
                  Parsec.char '0'
                    *> Parsec.choice
                      [ base 'b' "01" readBin Binary "binary",
                        base 'o' ['0' .. '7'] (fst . Unsafe.fromJust . viaNonEmpty head . readOct) Octal "octal",
                        base
                          'x'
                          (['0' .. '9'] ++ ['A' .. 'F'])
                          (fst . Unsafe.fromJust . viaNonEmpty head . readHex)
                          Hexadecimal
                          "hexadecimal"
                      ]
                Integer . IntegerLiteral (applySign sign value) hint <$> integerBits,
              do
                integer <- Parsec.many1 Parsec.digit
                mFraction <-
                  Parsec.optionMaybe $
                    Parsec.char '.' *> Parsec.many Parsec.digit
                mPower <-
                  Parsec.optionMaybe $
                    Parsec.oneOf "Ee"
                      *> ( (,)
                             <$> Parsec.optionMaybe (Parsec.oneOf "+-\x2212")
                             <*> Parsec.many1 Parsec.digit
                         )
                case (mFraction, mPower) of
                  (Nothing, Nothing) -> do
                    Integer . IntegerLiteral (applySign sign (fromMaybe 0 (readMaybe integer))) Decimal <$> integerBits
                  _float -> do
                    bits <-
                      Parsec.option Float64 $
                        Parsec.char 'f'
                          *> Parsec.choice
                            [ Float32 <$ Parsec.string "32",
                              Float64 <$ Parsec.string "64"
                            ]
                    return $
                      Float $
                        FloatLiteral
                          (applySign sign (fromMaybe 0 (readMaybe (integer ++ fromMaybe "" mFraction))))
                          (maybe 0 length mFraction)
                          (maybe 0 (\(s, p) -> applySign s (fromMaybe 0 (readMaybe p))) mPower)
                          bits
            ]
            <* Parsec.notFollowedBy Parsec.digit,
        Parsec.try $
          Arrow
            <$ Parsec.choice (map Parsec.string ["->", "\x2192"])
            <* Parsec.notFollowedBy symbol,
        let alphanumeric =
              (toText .) . (:)
                <$> (letter <|> Parsec.char '_')
                <*> (many . Parsec.choice) [letter, Parsec.char '_', Parsec.digit]
         in Parsec.choice
              [ do
                  name <- alphanumeric
                  return $ case name of
                    "about" -> About
                    "as" -> As
                    "case" -> Case
                    "define" -> Define
                    "do" -> Do
                    "elif" -> Elif
                    "else" -> Else
                    "if" -> If
                    "instance" -> Instance
                    "intrinsic" -> Intrinsic
                    "jump" -> Jump
                    "match" -> Match
                    "permission" -> Permission
                    "return" -> Return
                    "synonym" -> Synonym
                    "trait" -> Trait
                    "type" -> Type
                    "vocab" -> Vocab
                    "with" -> With
                    _ -> Word (Unqualified name),
                -- See note [Angle Brackets].

                bracketOperator '<',
                AngleBegin <$ Parsec.char '<',
                bracketOperator '>',
                AngleEnd <$ Parsec.char '>',
                Operator . Unqualified . toText <$> Parsec.many1 symbol
              ]
      ]

nestableCharacter :: Char -> Char -> Tokenizer [Char]
nestableCharacter open close = go
  where
    go =
      Parsec.choice
        [ (<?> "character") $ one <$> Parsec.noneOf ['\\', open, close],
          (\o t c -> o : t ++ [c])
            <$> (Parsec.char open <?> "nested opening quote")
            <*> (concat <$> Parsec.many go)
            <*> (Parsec.char close <?> "matching closing quote"),
          maybeToList <$> escape
        ]

letter :: Tokenizer Char
letter = Parsec.satisfy isLetter

readBin :: String -> Integer
readBin = go 0
  where
    go :: Integer -> String -> Integer
    go acc ds = case ds of
      '0' : ds' -> go (2 * acc + 0) ds'
      '1' : ds' -> go (2 * acc + 1) ds'
      [] -> acc
      _nonBinary -> error "non-binary digit"

text :: Tokenizer Text
text = toText . catMaybes <$> many (character '"')

nestableText :: Char -> Char -> Tokenizer Text
nestableText open close =
  toText . concat
    <$> many (nestableCharacter open close)

paragraph :: Tokenizer Text
paragraph = (<?> "paragraph") $ do
  _ <- Parsec.try $ Parsec.string "\"\"\""
  _ <- Parsec.endOfLine <?> "newline before paragraph body"
  (prefix, body) <- untilLeft paragraphLine
  body' <- forM body $ \line -> case Text.stripPrefix prefix line of
    Just line' -> return line'
    Nothing | Text.null line -> return ""
    _prefix ->
      Parsec.unexpected
        (Pretty.render $ Pretty.doubleQuotes $ Pretty.text $ toString line)
        -- HACK: Relies on formatting of messages to include "expected ...".
        <?> concat
          [ "all lines to be empty or begin with ",
            show $ BS.length (encodeUtf8 prefix),
            " spaces"
          ]
  return $ Text.intercalate "\n" body'

paragraphLine :: Tokenizer (Either Text Text)
paragraphLine =
  Parsec.choice
    [ (<?> "end of paragraph") $
        Parsec.try $
          Left . toText
            <$> Parsec.many (Parsec.char ' ')
            <* Parsec.string "\"\"\"",
      (<?> "paragraph line") $
        Right . toText . catMaybes
          <$> Parsec.many
            (Parsec.choice [Just <$> Parsec.noneOf ['\n', '\r'], escape])
          <* Parsec.endOfLine
    ]

untilLeft :: (Monad m) => m (Either a b) -> m (a, [b])
untilLeft p = go []
  where
    go acc = do
      mx <- p
      case mx of
        Left a -> return (a, reverse acc)
        Right b -> go (b : acc)
