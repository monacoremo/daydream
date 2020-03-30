{-# LANGUAGE DeriveGeneric #-}

module Events.Command (Command (..)) where

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
    deriving (Show, Eq, Generic)


instance FromJSON Command where
    parseJSON =
        Aeson.genericParseJSON options


instance ToJSON Command where
    toJSON =
        Aeson.genericToJSON options


options :: Aeson.Options
options =
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_'
        , Aeson.sumEncoding = Aeson.TaggedObject "command" "value"
        , Aeson.tagSingleConstructors = True
        }
