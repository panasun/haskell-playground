{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Types (GQLType, IORes, ResolveQ, ResolverQ (..), RootResolver (..), Undefined (..), liftEither)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Scotty (body, post, raw, scotty)

data Query m = Query
  { deity :: DeityArgs -> m Deity
  }
  deriving (Generic, GQLType)

data Deity = Deity
  { fullName :: Text, -- Non-Nullable Field
    power :: Maybe Text -- Nullable Field
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text, -- Required Argument
    mythology :: Maybe Text -- Optional Argument
  }
  deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ () IO Deity
resolveDeity DeityArgs {name, mythology} = liftEither $ askDB name mythology

askDB :: Text -> Maybe Text -> IO (Either String Deity)
askDB name _ = pure $ Right (Deity {fullName = name, power = Just "foobar"})

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = resolveDeity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)