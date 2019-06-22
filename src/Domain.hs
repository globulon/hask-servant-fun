{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain(User(..), Subscription(..)) where

import GHC.Generics
import Data.Time.Calendar

data User = User
  { name :: String
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

newtype Subscription = Subscription { id :: Int }
  deriving (Eq, Show, Generic)
