{-# LANGUAGE OverloadedStrings #-}
module EDN.Parser
  ( parseEDN
  , parseEDNFromText
  , ParseError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec hiding (ParseError, (<|>), many)
import Text.Parsec.Text
import Text.Parsec.Char
import Control.Applicative ((<|>), many)
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

import EDN.Types (EDNValue(..), ParseError(..))

type EDNParser = Parsec Text ()

parseEDN :: String -> Text -> Either ParseError EDNValue
parseEDN filename input = 
  case parse (whitespace *> ednValue <* eof) filename input of
    Left err -> Left $ ParseError (T.pack $ show err) 0
    Right val -> Right val

parseEDNFromText :: Text -> Either ParseError EDNValue
parseEDNFromText = parseEDN "<input>"

whitespace :: EDNParser ()
whitespace = skipMany (space <|> comment)

comment :: EDNParser Char
comment = char ';' *> manyTill anyChar (try newline <|> (eof >> return '\n')) >> return ' '

ednValue :: EDNParser EDNValue
ednValue = whitespace *> choice
  [ ednNil
  , ednBool
  , ednString
  , ednChar
  , ednNumber
  , ednKeyword
  , ednSymbol
  , ednList
  , ednVector
  , ednSet
  , ednMap
  , ednTagged
  ] <* whitespace

ednNil :: EDNParser EDNValue
ednNil = string "nil" >> return EDNNil

ednBool :: EDNParser EDNValue
ednBool = (string "true" >> return (EDNBool True))
      <|> (string "false" >> return (EDNBool False))

ednString :: EDNParser EDNValue
ednString = do
  char '"'
  content <- many stringChar
  char '"'
  return $ EDNString $ T.pack content
  where
    stringChar = (char '\\' >> escapeChar) <|> noneOf "\"\\"
    escapeChar = choice
      [ char '"' >> return '"'
      , char '\\' >> return '\\'
      , char 'n' >> return '\n'
      , char 't' >> return '\t'
      , char 'r' >> return '\r'
      ]

ednChar :: EDNParser EDNValue
ednChar = do
  char '\\'
  c <- choice
    [ try (string "newline") >> return '\n'
    , try (string "return") >> return '\r'
    , try (string "space") >> return ' '
    , try (string "tab") >> return '\t'
    , anyChar
    ]
  return $ EDNChar c

ednNumber :: EDNParser EDNValue
ednNumber = do
  numStr <- many1 (digit <|> char '.' <|> char '-' <|> char '+' <|> char 'e' <|> char 'E')
  case Sci.fromFloatDigits (read numStr :: Double) of
    num -> return $ EDNNumber num

ednKeyword :: EDNParser EDNValue
ednKeyword = do
  char ':'
  name <- many1 (alphaNum <|> oneOf ".-_+*/?$%&=<>")
  return $ EDNKeyword $ T.pack name

ednSymbol :: EDNParser EDNValue
ednSymbol = do
  first <- letter <|> oneOf ".-_+*/?$%&=<>"
  rest <- many (alphaNum <|> oneOf ".-_+*/?$%&=<>")
  return $ EDNSymbol $ T.pack (first:rest)

ednList :: EDNParser EDNValue
ednList = between (char '(' <* whitespace) (whitespace *> char ')') $ do
  vals <- many ednValue
  return $ EDNList vals

ednVector :: EDNParser EDNValue
ednVector = between (char '[' <* whitespace) (whitespace *> char ']') $ do
  vals <- many ednValue
  return $ EDNVector vals

ednSet :: EDNParser EDNValue
ednSet = do
  string "#{"
  whitespace
  vals <- many ednValue
  whitespace
  char '}'
  return $ EDNSet $ Set.fromList vals

ednMap :: EDNParser EDNValue
ednMap = between (char '{' <* whitespace) (whitespace *> char '}') $ do
  pairs <- many mapPair
  return $ EDNMap $ Map.fromList pairs
  where
    mapPair = do
      key <- ednValue
      val <- ednValue
      return (key, val)

ednTagged :: EDNParser EDNValue
ednTagged = do
  char '#'
  tag <- choice
    [ try (string "inst") >> return "inst"
    , try (string "uuid") >> return "uuid"
    , many1 (alphaNum <|> oneOf ".-_")
    ]
  whitespace
  val <- ednValue
  return $ EDNTagged (T.pack tag) val