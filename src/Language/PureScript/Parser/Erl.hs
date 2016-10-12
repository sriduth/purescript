module Language.PureScript.Parser.Erl (parseFile) where

import Prelude.Compat

import qualified Text.Parsec as P
import Text.Parsec ( (<|>) )
import qualified Text.Parsec.Char as PC

parseFile :: P.SourceName -> String -> Either P.ParseError [(String, Int)]
parseFile = P.parse parseLines

parseLines :: P.Parsec String u [(String, Int)]
parseLines = do
  lns <- P.many parseLine
  P.eof
  pure (concat lns)

parseLine :: P.Parsec String u [(String, Int)]
parseLine = P.try parseAttribute <|>
  do
    P.skipMany (PC.noneOf ['\n', '\r'])
    _ <- P.endOfLine
    pure []

parseAttribute :: P.Parsec String u [(String, Int)]
parseAttribute = attributeParser "export"
  (P.between (PC.char '[') (PC.char ']')
    (atomArityParser `P.sepBy` PC.char ','))

-- P.Parsec String u Token
--
attributeParser :: String -> P.Parsec String u a -> P.Parsec String u a
attributeParser name valueParser =
  -- PC.char '-' *> PC.string name *> P.between (PC.char '(') (PC.char ')') valueParser
  do
    _ <- PC.char '-'
    _ <- PC.string name
    res <- P.between (PC.char '(') (PC.char ')') valueParser
    _ <- PC.char '.'
    _ <- PC.endOfLine
    pure res

atomArityParser :: P.Parsec String u (String, Int)
atomArityParser = do
  PC.spaces
  a <- atomParser
  _ <- PC.char '/'
  n <- read <$> P.many1 PC.digit
  pure (a, n)

atomParser :: P.Parsec String u String
atomParser = quotedAtomParser <|> identifierParser

identifierParser :: P.Parsec String u String
identifierParser = do
  h <- PC.lower
  t <- P.many (PC.alphaNum <|> PC.char '_' <|> PC.char '@')
  pure (h:t)

quotedAtomParser :: P.Parsec String u String
quotedAtomParser = P.between (PC.char '\'') (PC.char '\'')
  (P.many1 (PC.noneOf ['\'', '\\'] <|> atomEscapedCharParser))

atomEscapedCharParser :: P.Parsec String u Char
atomEscapedCharParser = do
  _ <- PC.char '\\'
  PC.char '\'' <|> PC.char '\\'
