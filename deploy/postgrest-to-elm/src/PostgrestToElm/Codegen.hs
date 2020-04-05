{-# LANGUAGE OverloadedStrings #-}

module PostgrestToElm.Codegen
  ( fromApiStructure,
  )
where

import qualified Bound
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Data.Text.Manipulate (toPascal)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (vacuous)
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
  let typeName =
        toPascal $ apiTableName apiTable
      typeModuleName =
        rootModuleName ++ ["Types", typeName]
      endpointModuleName =
        rootModuleName ++ [typeName]
      typeQualified =
        Elm.Qualified typeModuleName
      endpointQualified =
        Elm.Qualified endpointModuleName
      getEndpoint :: Elm.Definition
      getEndpoint =
        Elm.Constant
          (endpointQualified "get")
          0
          (Bound.toScope $ vacuous $ ElmType.Global (endpointQualified "Test"))
          (Elm.List [])
      typeDef :: Elm.Definition
      typeDef =
        Elm.Alias
          (typeQualified typeName)
          0
          ( Bound.toScope $ vacuous $
              ElmType.Record
                [(Elm.Field "field", ElmType.Global (typeQualified "Test"))]
          )
      fieldType :: Elm.Definition
      fieldType =
        Elm.Alias
          (typeQualified "Field")
          0
          ( Bound.toScope $ vacuous $
              ElmType.Record
                [ ( Elm.Field "decoder",
                    ElmType.Global (Elm.Qualified ["Json", "Decode"] "Decoder")
                  ),
                  ( Elm.Field "encode",
                    ElmType.Global (Elm.Qualified ["Json", "Encode"] "Encoder")
                  ),
                  ( Elm.Field "name",
                    ElmType.Global (Elm.Qualified ["Basics"] "String")
                  )
                ]
          )
   in -- define fields as a type

      [ getEndpoint,
        typeDef,
        fieldType
      ]
