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
import qualified Language.Elm.Pretty as Elm
import qualified Language.Elm.Simplification as Elm
import qualified Language.Elm.Definition as Elm
import qualified Language.Elm.Type as ElmType
import qualified Language.Elm.Expression as Elm
import Data.Text.Prettyprint.Doc (Doc)
import qualified Bound
import Data.Void (vacuous)

import PostgrestToElm.ApiStructure (ApiStructure (..), ApiTable (..))


fromApiStructure :: ApiStructure -> [(Elm.Module, Doc Text)]
fromApiStructure apiStructure =
    modules $ apiToElm ["Api"] apiStructure


modules :: [Elm.Definition] -> [(Elm.Module, Doc Text)]
modules definitions =
    HashMap.toList . Elm.modules $ Elm.simplifyDefinition <$> definitions


apiToElm :: Elm.Module -> ApiStructure -> [Elm.Definition]
apiToElm modulePath apiStructure =
    concatMap (apiTableToElm modulePath) (apiTables apiStructure)


apiTableToElm :: Elm.Module -> ApiTable -> [Elm.Definition]
apiTableToElm rootModuleName apiTable =
    let
        moduleName =
            rootModuleName ++ [apiTableName apiTable]
    in
    [ Elm.Constant
        (Elm.Qualified moduleName "get")
        0
        (Bound.toScope $ vacuous $ ElmType.Global (Elm.Qualified moduleName "Test"))
        (Elm.List [])
    ]
