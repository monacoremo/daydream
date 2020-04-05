module Main where

import Events.Server (run)

main :: IO ()
main =
  do
    putStrLn "Running server on port 8080..."
    run 8080
