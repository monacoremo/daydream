{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
module Main where

import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Generics.SOP as SOP
import GHC.Generics
import Servant.API

import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import Servant.To.Elm
import Events.Server (API)
import Events.Command (Command (..))


main :: IO ()
main =
  let
    definitions =
      map (elmEndpointDefinition "Config.urlBase" ["Api", "Events"]) (elmEndpoints @API)
      <> jsonDefinitions @Command

    modules =
      Pretty.modules $
        Simplification.simplifyDefinition <$> definitions
  in
  forM_ (HashMap.toList modules) $ \(moduleName, contents) ->
    do
        print moduleName
        print contents
