{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}

module Events.Command (Command (..)) where

import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import qualified Generics.SOP as SOP
import Servant.API

import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)


data Command
    = CreateTree
        { rootLabel :: Text
        }
    | AppendLeaf
        { parentId :: Int
        }
    | DeleteNode
        { nodeId :: Int
        }
    | MoveNodeAppend
        { nodeId :: Int
        , parentId :: Int
        }
    deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo)


instance FromJSON Command where
    parseJSON =
        Aeson.genericParseJSON aesonOptions


instance ToJSON Command where
    toJSON =
        Aeson.genericToJSON aesonOptions


aesonOptions :: Aeson.Options
aesonOptions =
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_'
        , Aeson.sumEncoding = Aeson.TaggedObject "command" "value"
        , Aeson.tagSingleConstructors = True
        }


instance HasElmType Command where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Command defaultOptions "Api.Types.Command.Command"


instance HasElmDecoder Aeson.Value Command where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Command defaultOptions aesonOptions "Api.Types.Command.decoder"


instance HasElmEncoder Aeson.Value Command where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Command defaultOptions aesonOptions "Api.Types.Command.encoder"
