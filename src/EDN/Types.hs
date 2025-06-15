{-# LANGUAGE OverloadedStrings #-}
module EDN.Types
  ( EDNValue(..)
  , ParseError(..)
  , ReaderFunction
  , ReaderRegistry
  , ReaderError(..)
  , defaultReaders
  , applyReaders
  , toEDNText
  , escapeString
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)
import Data.Scientific (Scientific)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.UUID (UUID)
import qualified Data.UUID as UUID

data EDNValue
  = EDNNil
  | EDNBool Bool
  | EDNString Text
  | EDNChar Char
  | EDNNumber Scientific
  | EDNKeyword Text
  | EDNSymbol Text
  | EDNList [EDNValue]
  | EDNVector [EDNValue]
  | EDNSet (Set.Set EDNValue)
  | EDNMap (Map.Map EDNValue EDNValue)
  | EDNInstant UTCTime
  | EDNUuid UUID
  | EDNTagged Text EDNValue
  deriving (Show, Eq, Ord)

data ParseError = ParseError
  { errorMessage :: Text
  , errorPosition :: Int
  } deriving (Show, Eq)

data ReaderError = ReaderError
  { readerErrorMessage :: Text
  , readerErrorTag :: Text
  } deriving (Show, Eq)

-- | A reader function transforms a tagged literal's value into a specific type
type ReaderFunction = EDNValue -> Either ReaderError EDNValue

-- | Registry of reader functions by tag name  
type ReaderRegistry = Map.Map Text ReaderFunction

-- | Default readers for standard EDN tagged literals
defaultReaders :: ReaderRegistry
defaultReaders = Map.fromList
  [ ("inst", readInstant)
  , ("uuid", readUUID)
  ]

-- | Reader function for #inst tagged literals
readInstant :: ReaderFunction
readInstant (EDNString s) = 
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack s) of
    Just time -> Right (EDNInstant time)
    Nothing -> 
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack s) of
        Just time -> Right (EDNInstant time)
        Nothing -> Left (ReaderError "Invalid instant format" "inst")
readInstant _ = Left (ReaderError "Instant tag requires string value" "inst")

-- | Reader function for #uuid tagged literals  
readUUID :: ReaderFunction
readUUID (EDNString s) =
  case UUID.fromString (T.unpack s) of
    Just uuid -> Right (EDNUuid uuid)
    Nothing -> Left (ReaderError "Invalid UUID format" "uuid")
readUUID _ = Left (ReaderError "UUID tag requires string value" "uuid")

-- | Apply readers to transform tagged literals in an EDN value
applyReaders :: ReaderRegistry -> EDNValue -> Either ReaderError EDNValue
applyReaders readers = go
  where
    go (EDNTagged tag value) =
      case Map.lookup tag readers of
        Just reader -> reader value
        Nothing -> Right (EDNTagged tag value)  -- Unknown tag stays raw
    go (EDNList vals) = EDNList <$> mapM go vals
    go (EDNVector vals) = EDNVector <$> mapM go vals
    go (EDNSet s) = EDNSet . Set.fromList <$> mapM go (Set.toList s)
    go (EDNMap m) = do
      pairs <- mapM (\(k,v) -> (,) <$> go k <*> go v) (Map.toList m)
      return $ EDNMap $ Map.fromList pairs
    go other = Right other

-- | Convert EDN value to text representation
toEDNText :: EDNValue -> Text
toEDNText EDNNil = "nil"
toEDNText (EDNBool True) = "true"
toEDNText (EDNBool False) = "false"
toEDNText (EDNString s) = "\"" <> escapeString s <> "\""
toEDNText (EDNChar c) = "\\" <> T.singleton c
toEDNText (EDNNumber n) = T.pack (show n)
toEDNText (EDNKeyword k) = ":" <> k
toEDNText (EDNSymbol s) = s
toEDNText (EDNList vals) = "(" <> T.unwords (map toEDNText vals) <> ")"
toEDNText (EDNVector vals) = "[" <> T.unwords (map toEDNText vals) <> "]"
toEDNText (EDNSet s) = "#{" <> T.unwords (map toEDNText (Set.toList s)) <> "}"
toEDNText (EDNMap m) = "{" <> T.unwords (concatMap (\(k,v) -> [toEDNText k, toEDNText v]) (Map.toList m)) <> "}"
toEDNText (EDNInstant time) = "#inst \"" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" time) <> "\""
toEDNText (EDNUuid uuid) = "#uuid \"" <> T.pack (UUID.toString uuid) <> "\""
toEDNText (EDNTagged tag val) = "#" <> tag <> " " <> toEDNText val

-- | Escape special characters in strings
escapeString :: Text -> Text
escapeString = T.concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c = T.singleton c