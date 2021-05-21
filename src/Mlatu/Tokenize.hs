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
import Data.Char (isLetter, isLower, isPunctuation, isSymbol, isUpper)
import Data.Text qualified as Text
import Mlatu.Informer (Informer (..))
import Mlatu.Located (Located (..))
import Mlatu.Name (Unqualified (..))
import Mlatu.Origin qualified as Origin
import Mlatu.Report qualified as Report
import Mlatu.Token (Token (..))
import Prettyprinter (dquotes)
import Prettyprinter.Internal (Pretty (pretty))
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Parsec (Column, ParsecT, (<?>))
import Text.Parsec qualified as Parsec
import Text.Parsec.Pos qualified as Parsec

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
  pure $ At (Origin.range begin end) result

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
    Just c -> pure (Character c)
    Nothing -> Parsec.unexpected "empty character literal"

smartQuoteCharacterLiteral :: Tokenizer Token
smartQuoteCharacterLiteral = do
  mc <-
    Parsec.between (Parsec.char '\x2018') (Parsec.char '\x2019') $
      nestableCharacter '\x2018' '\x2019'
  case mc of
    [c] -> pure (Character c)
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
        _ -> pure $ Just c

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
    *> Parsec.choice (fmap Parsec.satisfy [isSymbol, isPunctuation])

special :: Tokenizer Char
special = Parsec.oneOf "\"'(),:[\\]_{}"

comma :: Tokenizer Token
comma = Comma <$ Parsec.char ','

dot :: Tokenizer Token
dot = Dot <$ Parsec.char '.'

ignore :: Tokenizer Token
ignore = Parsec.try $ Ignore <$ Parsec.char '_' <* Parsec.notFollowedBy letter

colon :: Tokenizer Token
colon = Colon <$ Parsec.char ':'

caseToken :: Tokenizer Token
caseToken = Case <$ Parsec.char '|'

vectorBegin :: Tokenizer Token
vectorBegin = VectorBegin <$ Parsec.char '['

vectorEnd :: Tokenizer Token
vectorEnd = VectorEnd <$ Parsec.char ']'

reference :: Tokenizer Token
reference = Reference <$ Parsec.char '\\'

num :: Tokenizer Token
num = Integer . Unsafe.read <$> Parsec.many1 Parsec.digit

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
      <$ Parsec.choice (fmap Parsec.string ["->", "\x2192"])
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
            <$> lower
            <*> many (Parsec.choice [letter, Parsec.char '-', Parsec.digit])
        pure $ case name of
          "alias" -> Alias
          "about" -> About
          "as" -> As
          "codata" -> Codata
          "data" -> Data
          "define" -> Define
          "else" -> Else
          "field" -> Field
          "for" -> For
          "if" -> If
          "instance" -> Instance
          "match" -> Match
          "module" -> Module
          "permission" -> Permission
          "trait" -> Trait
          "with" -> With
          "where" -> Where
          _ -> LowerWord (Unqualified name),
      UpperWord . Unqualified
        <$> ( (toText .) . (:)
                <$> upper
                <*> many (Parsec.choice [letter, Parsec.char '-', Parsec.digit])
            ),
      angleBegin,
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
        caseToken,
        dot,
        groupBegin,
        groupEnd,
        ignore,
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
            <*> (asum <$> Parsec.many go)
            <*> (Parsec.char close <?> "matching closing quote"),
          maybeToList <$> escape
        ]

letter :: Tokenizer Char
letter = Parsec.satisfy isLetter

lower :: Tokenizer Char
lower = Parsec.satisfy isLower

upper :: Tokenizer Char
upper = Parsec.satisfy isUpper

text :: Tokenizer Text
text = toText . catMaybes <$> many (character '"')

nestableText :: Char -> Char -> Tokenizer Text
nestableText open close =
  toText . asum
    <$> many (nestableCharacter open close)

paragraph :: Tokenizer Token
paragraph =
  (<?> "paragraph") $
    Text <$> do
      _ <- Parsec.try $ Parsec.string "\"\"\""
      _ <- Parsec.endOfLine <?> "newline before paragraph body"
      (prefix, body) <- untilLeft paragraphLine
      body' <- forM body $ \line -> case Text.stripPrefix prefix line of
        Just line' -> pure line'
        Nothing | Text.null line -> pure ""
        _prefix ->
          Parsec.unexpected
            (show $ dquotes $ pretty line)
            -- HACK: Relies on formatting of messages to include "expected ..".
            <?> asum
              [ "all lines to be empty or begin with ",
                show $ BS.length (encodeUtf8 prefix),
                " spaces"
              ]
      pure $ Text.intercalate "\n" body'

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
        Left a -> pure (a, reverse acc)
        Right b -> go (b : acc)
