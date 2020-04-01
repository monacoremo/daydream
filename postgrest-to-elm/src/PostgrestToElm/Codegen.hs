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
        moduleName =
            rootModuleName ++ [toPascal $ apiTableName apiTable]

        qualified =
            Elm.Qualified moduleName
    in
    [ Elm.Constant
        (qualified "get")
        0
        (Bound.toScope $ vacuous $ ElmType.Global (qualified "Test"))
        (Elm.List [])
    ]
