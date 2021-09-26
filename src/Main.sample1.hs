{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import API (api)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Monoid (mconcat, (<>))
import Data.Time (getCurrentTime)
import GHC.Generics
import Network.HTTP.Types
import System.IO (readFile)
import Web.Scotty
import Word (word)
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

matchesUsername :: String -> User -> Bool
matchesUsername name user = userName user == name

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
  get "/userNames/:name" $ do
    id <- param "name"
    json (filter (matchesUsername id) allUsers)

-- get "/api" $ do
--   raw =<< (liftIO . api =<< body)

main :: IO ()
main = do
  contents <- readFile "src/data.json"
  putStrLn contents
  print $ (3 + 4)
  time <- getCurrentTime
  print time
  putStrLn "Starting Server..."
  print (word "bin")
  apiM <- api
  print apiM
  scotty 3000 routes

-- scotty 3000 $ post "/api" $ raw =<< "test"
