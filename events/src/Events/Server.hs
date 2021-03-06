{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Events.Server
  ( API,
    run,
  )
where

import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Data.Text (Text)
import Events.Command (Command)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API ((:>), Header, JSON, Post, ReqBody)
import Servant.Server (Application, Handler, Server, serve)

type API = Header "Cookie" Text :> ReqBody '[JSON] Command :> Post '[JSON] Text

server :: Server API
server = commandHandler

commandHandler :: Maybe Text -> Command -> Handler Text
commandHandler maybeCookie command =
  return "test"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

run :: Int -> IO ()
run port =
  Warp.run port app
