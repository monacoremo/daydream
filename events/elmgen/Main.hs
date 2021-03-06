{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative ((<|>))
import Data.Foldable (mapM_)
import qualified Data.HashMap.Lazy as HashMap
import Data.Semigroup ((<>))
import Data.Text (Text, append, intercalate, unpack)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc (Doc)
import Events.Command (Command (..))
import Events.Server (API)
import qualified Language.Elm.Definition as Elm
import qualified Language.Elm.Name as Elm
import qualified Language.Elm.Pretty as Elm
import qualified Language.Elm.Simplification as Elm
import qualified Language.Haskell.To.Elm as HaskellToElm
import qualified Options.Applicative as OptParse
import qualified Servant.To.Elm as ServantToElm
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath.Posix as FilePath

main :: IO ()
main =
  run =<< OptParse.execParser info

run :: Options -> IO ()
run options =
  let op =
        case options of
          FileOutput targetDir ->
            writeElmModule targetDir
          PrintOutput ->
            printElmModule
   in mapM_ (uncurry op) modules

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

-- ELM MODULES

definitions :: [Elm.Definition]
definitions =
  map
    (ServantToElm.elmEndpointDefinition "Config.urlBase" ["Generated", "Api", "Events"])
    (ServantToElm.elmEndpoints @API)
    <> HaskellToElm.jsonDefinitions @Command

modules :: [(Elm.Module, Doc Text)]
modules =
  HashMap.toList . Elm.modules $ Elm.simplifyDefinition <$> definitions

-- CLI OPTIONS PARSER

data Options
  = FileOutput FilePath
  | PrintOutput
  deriving (Show)

info :: OptParse.ParserInfo Options
info =
  OptParse.info parser $
    OptParse.progDesc "Generate Elm bindings for the events API"

parser :: OptParse.Parser Options
parser =
  fileOutput <|> printOutput

fileOutput :: OptParse.Parser Options
fileOutput =
  FileOutput
    <$> OptParse.strOption
      ( OptParse.long "target-directory"
          <> OptParse.metavar "TARGETDIR"
          <> OptParse.help "Target directory for the generated Elm modules"
      )

printOutput :: OptParse.Parser Options
printOutput =
  OptParse.flag'
    PrintOutput
    ( OptParse.long "print"
        <> OptParse.help "Print generated Elm modules to stdout"
    )
