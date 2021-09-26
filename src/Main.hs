{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Network.HTTP.Types
import Web.Scotty
import Prelude

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    text "This was a GET request!"
  get "/test" $ do
    text "/test"
  post "/test-post" $ do
    text "/test-post"
  get "/word/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- post "/api" $ do
--   raw =<< (liftIO . api =< body)
