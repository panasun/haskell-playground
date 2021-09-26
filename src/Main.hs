{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Monoid (mconcat, (<>))
import GHC.Generics
import Network.HTTP.Types
import Web.Scotty
import Prelude

data User = User {userId :: Int, userName :: String} deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

bob :: User
bob = User {userId = 1, userName = "bob"}

jenny :: User
jenny = User {userId = 2, userName = "jenny"}

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

routes :: ScottyM ()
routes = do
  get "/hello/:name" $ do
    name <- param "name"
    text ("hello " <> name <> "!")
  get "/users" $ do
    json allUsers
  get "/users/:id" $ do
    id <- param "id"
    json (filter (matchesId id) allUsers)

main :: IO ()
main = do
  scotty 3000 routes
