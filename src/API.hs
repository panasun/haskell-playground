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

module API (api) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined (..))
import Data.Text (Text)
import Main (Deity)

importGQLDocument "schema.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    deity DeityArgs {name} =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting")
          }

api :: ByteString -> IO ByteString
api = interpreter rootResolver

newtype Query m = Query
  { deity :: DeityArgs -> m Deity
  }
  deriving (Generic, GQLType)

data Deity = Deity
  { fullName :: Text,
    power :: Maybe Text
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text,
    mythology :: Maybe Text
  }
  deriving (Generic)

resolveDeity :: DeityArgs -> ResolverQ e () Deity
resolveDeity DeityArgs {name, mythology} = liftEither $ dbDeity name mythology mythology
