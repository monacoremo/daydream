{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Events.Command
  ( Command (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.API
import Servant.To.Elm

data Command
  = CreateTree
      { rootLabel :: Text
      }
  | AppendLeaf
      { parentId :: Int,
        label :: Text
      }
  | DeleteNode
      { nodeId :: Int
      }
  | MoveNodeAppend
      { nodeId :: Int,
        parentId :: Int
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
      --, Aeson.sumEncoding = Aeson.TaggedObject "command" "value"
      --, Aeson.tagSingleConstructors = True
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
