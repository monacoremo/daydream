{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative ((<|>))
import Data.Foldable (mapM_)
import qualified Data.HashMap.Lazy as HashMap
import Data.Semigroup ((<>))
import Data.Text (Text, append, intercalate, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (Doc)
import qualified Hasql.Connection as Connection
import qualified Language.Elm.Definition as Elm
import qualified Language.Elm.Name as Elm
import qualified Language.Elm.Pretty as Elm
import qualified Language.Elm.Simplification as Elm
import qualified Options.Applicative as OptParse
import qualified PostgrestToElm.ApiStructure as ApiStructure
import qualified PostgrestToElm.Codegen as Codegen
import qualified PostgrestToElm.DbStructure as DbStructure
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath.Posix as FilePath

main :: IO ()
main =
  run =<< OptParse.execParser info

run :: Options -> IO ()
run options =
  let outputOp =
        case output options of
          FileOutput targetDir ->
            writeElmModule targetDir
          PrintOutput ->
            printElmModule
   in do
        Right connection <- Connection.acquire (encodeUtf8 $ dburi options)
        dbStructure <-
          DbStructure.get
            connection
            [schema options]
            (role options)
        apiStructure <-
          ApiStructure.fromDbStructure
            (schema options)
            connection
            dbStructure
        mapM_ (uncurry outputOp) (Codegen.fromApiStructure apiStructure)

writeElmModule :: FilePath -> Elm.Module -> Doc Text -> IO ()
writeElmModule targetDir moduleName contents =
  let modulePath =
        map unpack moduleName
      dirPath =
        FilePath.joinPath (targetDir : init modulePath)
      filePath =
        FilePath.joinPath [dirPath, last modulePath ++ ".elm"]
   in do
        createDirectoryIfMissing True dirPath
        writeFile filePath . show $ contents

printElmModule :: Elm.Module -> Doc Text -> IO ()
printElmModule moduleName contents =
  do
    Text.putStrLn ""
    Text.putStrLn $ append "### Module " (intercalate "." moduleName)
    Text.putStrLn ""
    print contents
    Text.putStrLn ""

-- CLI OPTIONS PARSER

data Options
  = Options
      { dburi :: Text,
        schema :: Text,
        role :: Text,
        output :: OutputOptions
      }

data OutputOptions
  = FileOutput FilePath
  | PrintOutput
  deriving (Show)

info :: OptParse.ParserInfo Options
info =
  OptParse.info parser $
    OptParse.progDesc "Generate Elm bindings for a PostgREST API"

parser :: OptParse.Parser Options
parser =
  Options
    <$> OptParse.strOption
      ( OptParse.long "db-uri"
          <> OptParse.metavar "DBURI"
          <> OptParse.help "URI for the Postgres database connection"
      )
    <*> OptParse.strOption
      ( OptParse.long "schema"
          <> OptParse.metavar "SCHEMA"
          <> OptParse.help "Name of the database schema"
      )
    <*> OptParse.strOption
      ( OptParse.long "role"
          <> OptParse.metavar "ROLE"
          <> OptParse.help "Name of the database role"
      )
    <*> outputOption

outputOption :: OptParse.Parser OutputOptions
outputOption =
  printOutput <|> fileOutput

fileOutput :: OptParse.Parser OutputOptions
fileOutput =
  FileOutput
    <$> OptParse.strOption
      ( OptParse.long "target-directory"
          <> OptParse.metavar "TARGETDIR"
          <> OptParse.help "Target directory for the generated Elm modules"
      )

printOutput :: OptParse.Parser OutputOptions
printOutput =
  OptParse.flag'
    PrintOutput
    ( OptParse.long "print"
        <> OptParse.help "Print generated Elm modules to stdout"
    )
