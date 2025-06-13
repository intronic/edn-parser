{-# LANGUAGE OverloadedStrings #-}
module EDN.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Scientific (Scientific)
import qualified Data.Map as Map
import qualified Data.Set as Set

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
  | EDNUuid Text
  | EDNTagged Text EDNValue
  deriving (Show, Eq, Ord)

data ParseError = ParseError
  { errorMessage :: Text
  , errorPosition :: Int
  } deriving (Show, Eq)