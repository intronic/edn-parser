{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.IO (stdout)

import EDN.CodeGen.JavaScript (generateJavaScript)
import EDN.CodeGen.TypeScript (generateTypeScript)

data Options = Options
  { optLanguage :: String
  , optOutput :: Maybe String
  } deriving (Show)

options :: Parser Options
options = Options
  <$> strOption
      ( long "language"
     <> short 'l'
     <> metavar "LANG"
     <> help "Target language (js or ts)"
     <> value "js" )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file (default: stdout)" ))

main :: IO ()
main = do
  opts <- execParser $ info (options <**> helper)
    ( fullDesc
   <> progDesc "Generate JavaScript/TypeScript EDN parser libraries"
   <> header "edn-codegen - EDN parser code generator" )
  
  let code = case optLanguage opts of
        "ts" -> generateTypeScript
        "typescript" -> generateTypeScript
        _ -> generateJavaScript
  
  case optOutput opts of
    Just filename -> TIO.writeFile filename code
    Nothing -> TIO.hPutStr stdout code