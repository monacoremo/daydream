{-# LANGUAGE OverloadedStrings #-}

module PostgrestToElm.Codegen
    ( fromApiStructure
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import PostgREST.DbStructure (getDbStructure, getPgVersion)
import System.Environment (getArgs)
import GHC.Int (Int32)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Text
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as HT
import qualified PostgREST.Types as PostgREST
import qualified Data.Maybe as Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Elm.Name as Elm
import Data.Text.Prettyprint.Doc (Doc)

import PostgrestToElm.ApiStructure (ApiStructure (..), ApiTable (..))


fromApiStructure :: ApiStructure -> [(Elm.Module, Doc Text)]
fromApiStructure apiStructure =
    []


data ElmModule =
    ElmModule
        { elmModuleName :: Text
        , elmModulePath :: [Text]
        , elmModuleDeclarations :: [ElmDeclaration]
        }
        deriving (Show, Eq)

data ElmDeclaration =
    ElmDeclaration
        { elmDeclarationName :: Text
        , elmDeclarationExposed :: Bool
        , elmDeclarationType :: Text
        , elmDeclarationParams :: [Text]
        , elmDeclarationExpr :: Text
        }
        deriving (Show, Eq)


apiToElm :: [Text] -> ApiStructure -> [ElmModule]
apiToElm modulePath apiStructure =
    concatMap (apiTableToElm modulePath) (apiTables apiStructure)


apiTableToElm :: [Text] -> ApiTable -> [ElmModule]
apiTableToElm modulePath apiTable =
    [ ElmModule
        { elmModulePath = modulePath ++ ["Types"]
        , elmModuleName = apiTableName apiTable
        , elmModuleDeclarations = []
        }
    ]

elmModuleCode :: ElmModule -> Text
elmModuleCode elmModule =
    Text.concat
        [ elmModuleName elmModule
        , "\ntest"
        ]

elmModuleFilePath :: ElmModule -> FilePath
elmModuleFilePath elmModule =
    Text.unpack . Text.intercalate "/" $ elmModulePath elmModule

elmModuleFileName :: ElmModule -> FilePath
elmModuleFileName elmModule =
    elmModuleFilePath elmModule
    ++ "/" ++ Text.unpack (elmModuleName elmModule) ++ ".elm"

writeElmModule :: FilePath -> ElmModule -> IO ()
writeElmModule basePath elmModule =
    let
        dir =
            basePath ++ Text.unpack (Text.intercalate "/" (elmModulePath elmModule))
    in
    do
        createDirectoryIfMissing True dir

        writeFile
            (dir ++ "/" ++ Text.unpack (elmModuleName elmModule) ++ ".elm")
            (Text.unpack (elmModuleCode elmModule))
