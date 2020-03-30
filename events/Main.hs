{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Events.Command as Command
import Data.Aeson (encode)


main :: IO ()
main = print $ encode $ Command.CreateTree "test"
