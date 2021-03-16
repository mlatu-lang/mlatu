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
import Mlatu.Informer (Informer (..))
import Mlatu.Literal (FloatLiteral (..), IntegerLiteral (..), Base(..))
import Mlatu.Located (Located (..))
import Mlatu.Name (Unqualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Report qualified as Report
import Mlatu.Token (Token (..))
import Numeric (readHex, readOct)
import Prettyprinter (dquotes)
import Prettyprinter.Internal (Pretty (pretty))
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Parsec (Column, ParsecT, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos qualified as Parsec
import Mlatu.Ice (ice)

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
  m [Located Token]
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

fileTokenizer :: Tokenizer [Located Token]
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

tokensTokenizer :: Tokenizer [Located Token]
tokensTokenizer = tokenTokenizer `Parsec.sepEndBy` silenceTokenizer

rangedTokenizer :: Tokenizer Token -> Tokenizer (Located Token)
rangedTokenizer parser = do
  begin <- Parsec.getPosition
  result <- parser
  end <- Parsec.getPosition
  return $ At (Origin.range begin end) result

blockBegin :: Tokenizer Token
blockBegin = BlockBegin <$ Parsec.char '{'

blockEnd :: Tokenizer Token
blockEnd = BlockEnd <$ Parsec.char '}'

groupBegin :: Tokenizer Token
groupBegin = GroupBegin <$ Parsec.char '('

groupEnd :: Tokenizer Token
groupEnd = GroupEnd <$ Parsec.char ')'

singleQuote :: Tokenizer Char
singleQuote = Parsec.char '\''

singleQuoteCharacterLiteral :: Tokenizer Token
singleQuoteCharacterLiteral = do
  mc <- Parsec.between singleQuote singleQuote $ character '\''
  case mc of
    Just c -> return (Character c)
    Nothing -> Parsec.unexpected "empty character literal"

smartQuoteCharacterLiteral :: Tokenizer Token
smartQuoteCharacterLiteral = do
  mc <-
    Parsec.between (Parsec.char '\x2018') (Parsec.char '\x2019') $
      nestableCharacter '\x2018' '\x2019'
  case mc of
    [c] -> return (Character c)
    [] -> Parsec.unexpected "empty character literal"
    _multiLit -> Parsec.unexpected "multi-character literal"

characterLiteral :: Tokenizer Token
characterLiteral = singleQuoteCharacterLiteral <|> smartQuoteCharacterLiteral

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

comma :: Tokenizer Token
comma = Comma <$ Parsec.char ','

ellipsis :: Tokenizer Token
ellipsis =
  Parsec.try $
    Ellipsis
      <$ Parsec.choice (map Parsec.string ["...", "\x2026"])

ignore :: Tokenizer Token
ignore = Parsec.try $ Ignore <$ Parsec.char '_' <* Parsec.notFollowedBy letter

vocabLookup :: Tokenizer Token
vocabLookup =
  Parsec.try $
    VocabLookup
      <$ Parsec.choice (map Parsec.string ["::", "\x2237"])

colon :: Tokenizer Token
colon = Colon <$ Parsec.char ':'

vectorBegin :: Tokenizer Token
vectorBegin = VectorBegin <$ Parsec.char '['

vectorEnd :: Tokenizer Token
vectorEnd = VectorEnd <$ Parsec.char ']'

reference :: Tokenizer Token
reference = Reference <$ Parsec.char '\\'

num :: Tokenizer Token
num = Parsec.try $ do
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
  Parsec.choice
    [ Parsec.try $
        (\(hint, value) -> Integer (IntegerLiteral (applySign sign value) hint))
          <$> ( Parsec.char '0'
                  *> Parsec.choice
                    [ base 'b' ['0' .. '1'] readBin Binary "binary",
                      base 'o' ['0' .. '7'] (fst . Unsafe.fromJust . viaNonEmpty head . readOct) Octal "octal",
                      base
                        'x'
                        (['0' .. '9'] ++ ['A' .. 'F'])
                        (fst . Unsafe.fromJust . viaNonEmpty head . readHex)
                        Hexadecimal
                        "hexadecimal"
                    ]
              ),
      ( \integer fraction power ->
          case (fraction, power) of
            (Nothing, Nothing) -> Integer (IntegerLiteral (applySign sign (fromMaybe 0 (readMaybe integer))) Decimal)
            (_, _) ->
              Float
                ( FloatLiteral
                    (applySign sign (fromMaybe 0 (readMaybe (integer ++ fromMaybe "" fraction))))
                    (maybe 0 length fraction)
                    (maybe 0 (\(s, p) -> applySign s (fromMaybe 0 (readMaybe p))) power)
                )
      )
        <$> Parsec.many1 Parsec.digit
        <*> Parsec.optionMaybe
          ( Parsec.char '.' *> Parsec.many Parsec.digit
          )
        <*> Parsec.optionMaybe
          ( Parsec.oneOf "Ee"
              *> ( (,)
                     <$> Parsec.optionMaybe (Parsec.oneOf "+-\x2212")
                     <*> Parsec.many1 Parsec.digit
                 )
          )
    ]
    <* Parsec.notFollowedBy Parsec.digit

doubleQuoteStringLiteral :: Tokenizer Token
doubleQuoteStringLiteral =
  Text
    <$> Parsec.between
      (Parsec.char '"')
      (Parsec.char '"' <?> "closing double quote")
      text

smartQuoteStringLiteral :: Tokenizer Token
smartQuoteStringLiteral =
  Text
    <$> Parsec.between
      (Parsec.char '\x201C')
      (Parsec.char '\x201D' <?> "closing right double quote")
      (nestableText '\x201C' '\x201D')

stringLiteral :: Tokenizer Token
stringLiteral = doubleQuoteStringLiteral <|> smartQuoteStringLiteral

arrow :: Tokenizer Token
arrow =
  Parsec.try $
    Arrow
      <$ Parsec.choice (map Parsec.string ["->", "\x2192"])
      <* Parsec.notFollowedBy symbol

angleBegin :: Tokenizer Token
angleBegin = AngleBegin <$ Parsec.char '<'

angleEnd :: Tokenizer Token
angleEnd = AngleEnd <$ Parsec.char '>'

alphanumeric :: Tokenizer Token
alphanumeric =
  Parsec.choice
    [ do
        name <-
          (toText .) . (:)
            <$> (letter <|> Parsec.char '_')
            <*> (many . Parsec.choice)
              [letter, Parsec.char '_', Parsec.digit]
        return $ case name of
          "about" -> About
          "as" -> As
          "case" -> Case
          "define" -> Define
          "do" -> Do
          "else" -> Else
          "if" -> If
          "instance" -> Instance
          "intrinsic" -> Intrinsic
          "match" -> Match
          "permission" -> Permission
          "return" -> Return
          "trait" -> Trait
          "type" -> Type
          "vocab" -> Vocab
          "with" -> With
          "where" -> Where
          _ -> Word (Unqualified name),
      -- See note [Angle Brackets].

      Operator (Unqualified (one '<'))
        <$ Parsec.try (Parsec.char '<' <* Parsec.notFollowedBy symbol),
      angleBegin,
      Operator (Unqualified (one '>'))
        <$ Parsec.try (Parsec.char '>' <* Parsec.notFollowedBy symbol),
      angleEnd,
      operator
    ]

operator :: Tokenizer Token
operator = Operator . Unqualified . toText <$> Parsec.many1 symbol

tokenTokenizer :: Tokenizer (Located Token)
tokenTokenizer =
  rangedTokenizer $
    Parsec.choice
      [ blockBegin,
        blockEnd,
        characterLiteral,
        comma,
        ellipsis,
        groupBegin,
        groupEnd,
        ignore,
        vocabLookup,
        colon,
        vectorBegin,
        vectorEnd,
        reference,
        paragraph,
        stringLiteral,
        num,
        arrow,
        alphanumeric
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
      _nonBinary -> ice "Mlatu.Tokenize.readBin non-binary digit"

text :: Tokenizer Text
text = toText . catMaybes <$> many (character '"')

nestableText :: Char -> Char -> Tokenizer Text
nestableText open close =
  toText . concat
    <$> many (nestableCharacter open close)

paragraph :: Tokenizer Token
paragraph =
  (<?> "paragraph") $
    Text <$> do
      _ <- Parsec.try $ Parsec.string "\"\"\""
      _ <- Parsec.endOfLine <?> "newline before paragraph body"
      (prefix, body) <- untilLeft paragraphLine
      body' <- forM body $ \line -> case Text.stripPrefix prefix line of
        Just line' -> return line'
        Nothing | Text.null line -> return ""
        _prefix ->
          Parsec.unexpected
            (show $ dquotes $ pretty line)
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
