{-# LANGUAGE OverloadedStrings #-}

module PostgrestToElm.Codegen
    ( fromApiStructure
    ) where

import Data.Text (Text)
import Data.Text.Manipulate (toPascal)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (vacuous)
import qualified Bound
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Language.Elm.Definition as Elm
import qualified Language.Elm.Expression as Elm
import qualified Language.Elm.Name as Elm
import qualified Language.Elm.Pretty as Elm
import qualified Language.Elm.Simplification as Elm
import qualified Language.Elm.Type as ElmType


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
        typeName =
            toPascal $ apiTableName apiTable

        typeModuleName =
            rootModuleName ++ ["Types", typeName]

        endpointModuleName =
            rootModuleName ++ [typeName]

        typeQualified =
            Elm.Qualified typeModuleName

        endpointQualified =
            Elm.Qualified endpointModuleName
    in
    [ Elm.Constant
        (endpointQualified "get")
        0
        (Bound.toScope $ vacuous $ ElmType.Global (endpointQualified "Test"))
        (Elm.List [])
    , Elm.Alias
        (typeQualified typeName)
        0
        (Bound.toScope $ vacuous $ ElmType.Record
            [(Elm.Field "field", ElmType.Global (typeQualified "Test"))]
        )
    ]
