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
whitespace = skipMany (space <|> char ',' <|> comment)

comment :: EDNParser Char
comment = char ';' *> manyTill anyChar (try newline <|> (eof >> return '\n')) >> return ' '

ednValue :: EDNParser EDNValue
ednValue = whitespace *> choice
  [ ednNil
  , ednBool
  , ednString
  , ednChar
  , try ednNumber
  , ednKeyword
  , ednList
  , ednVector
  , ednMap
  , ednTaggedOrSet
  , ednSymbol
  ] <* whitespace

ednNil :: EDNParser EDNValue
ednNil = try $ do
  string "nil"
  notFollowedBy (alphaNum <|> oneOf ".-_+*/?$%&=<>")
  return EDNNil

ednBool :: EDNParser EDNValue
ednBool = try $ choice
  [ string "true" >> notFollowedBy (alphaNum <|> oneOf ".-_+*/?$%&=<>") >> return (EDNBool True)
  , string "false" >> notFollowedBy (alphaNum <|> oneOf ".-_+*/?$%&=<>") >> return (EDNBool False)
  ]

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
  sign <- optionMaybe (char '+' <|> char '-')
  intPart <- many1 digit
  fracPart <- optionMaybe (char '.' *> many1 digit)
  expPart <- optionMaybe $ do
    _ <- char 'e' <|> char 'E'
    expSign <- optionMaybe (char '+' <|> char '-')
    expDigits <- many1 digit
    return (expSign, expDigits)
  
  let signStr = maybe "" (:[]) sign
      intStr = intPart
      fracStr = maybe "" (\f -> "." ++ f) fracPart
      expStr = case expPart of
        Nothing -> ""
        Just (expSign, expDigits) -> 
          "e" ++ maybe "" (:[]) expSign ++ expDigits
      numStr = signStr ++ intStr ++ fracStr ++ expStr
  
  case readMaybe numStr of
    Just d -> return $ EDNNumber (Sci.fromFloatDigits d)
    Nothing -> fail $ "Invalid number: " ++ numStr
  where
    readMaybe :: String -> Maybe Double
    readMaybe s = 
      let s' = if take 1 s == "+" then drop 1 s else s
      in case reads s' of
        [(x, "")] -> Just x
        _ -> Nothing

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

ednTaggedOrSet :: EDNParser EDNValue
ednTaggedOrSet = do
  char '#'
  choice
    [ char '{' *> (ednSetBody >>= return . EDNSet . Set.fromList)
    , ednTaggedBody
    ]
  where
    ednSetBody = do
      whitespace
      vals <- many ednValue
      whitespace
      char '}'
      return vals
    
    ednTaggedBody = do
      tag <- choice
        [ try (string "inst") >> return "inst"
        , try (string "uuid") >> return "uuid"
        , many1 (alphaNum <|> oneOf ".-_")
        ]
      whitespace
      val <- ednValue
      return $ EDNTagged (T.pack tag) val

ednMap :: EDNParser EDNValue
ednMap = between (char '{' <* whitespace) (whitespace *> char '}') $ do
  pairs <- many mapPair
  return $ EDNMap $ Map.fromList pairs
  where
    mapPair = do
      key <- ednValue
      val <- ednValue
      return (key, val)

