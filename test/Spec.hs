{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map

import EDN.Parser
import EDN.Types

main :: IO ()
main = hspec $ do
  describe "EDN Parser" $ do
    describe "Simple values" $ do
      it "parses nil" $ do
        parseEDNFromText "nil" `shouldBe` Right EDNNil
      
      it "parses true" $ do
        parseEDNFromText "true" `shouldBe` Right (EDNBool True)
      
      it "parses false" $ do
        parseEDNFromText "false" `shouldBe` Right (EDNBool False)
      
      it "parses integers" $ do
        case parseEDNFromText "42" of
          Right (EDNNumber n) -> show n `shouldBe` "42.0"
          _ -> expectationFailure "Expected number"
      
      it "parses strings" $ do
        parseEDNFromText "\"hello\"" `shouldBe` Right (EDNString "hello")
      
      it "parses keywords" $ do
        parseEDNFromText ":keyword" `shouldBe` Right (EDNKeyword "keyword")
      
      it "parses symbols" $ do
        parseEDNFromText "symbol" `shouldBe` Right (EDNSymbol "symbol")
    
    describe "Collections" $ do
      it "parses empty vector" $ do
        parseEDNFromText "[]" `shouldBe` Right (EDNVector [])
      
      it "parses vector with elements" $ do
        parseEDNFromText "[1 2 3]" `shouldSatisfy` isVectorWithThreeNumbers
      
      it "parses empty list" $ do
        parseEDNFromText "()" `shouldBe` Right (EDNList [])
      
      it "parses empty map" $ do
        parseEDNFromText "{}" `shouldBe` Right (EDNMap Map.empty)
      
      it "parses map with key-value pairs" $ do
        case parseEDNFromText "{:key \"value\"}" of
          Right (EDNMap m) -> Map.size m `shouldBe` 1
          _ -> expectationFailure "Expected map"
      
      it "parses empty set" $ do
        parseEDNFromText "#{}" `shouldBe` Right (EDNSet Set.empty)
    
    describe "Complex structures" $ do
      it "parses nested collections" $ do
        let input = "{:users [{:name \"Alice\"}]}"
        case parseEDNFromText input of
          Right (EDNMap _) -> return ()
          _ -> expectationFailure "Expected nested structure to parse"
      
      it "handles whitespace and comments" $ do
        let input = "; comment\n  42  ; another comment"
        case parseEDNFromText input of
          Right (EDNNumber _) -> return ()
          _ -> expectationFailure "Expected number with comments"

isVectorWithThreeNumbers :: Either ParseError EDNValue -> Bool
isVectorWithThreeNumbers (Right (EDNVector [EDNNumber _, EDNNumber _, EDNNumber _])) = True
isVectorWithThreeNumbers _ = False