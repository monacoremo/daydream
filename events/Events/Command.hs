{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Events.Command (Command (..)) where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
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
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
