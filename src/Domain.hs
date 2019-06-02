{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain(User(..), CounterVal(..)) where

import GHC.Generics
import Data.Time.Calendar

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)


newtype CounterVal = CounterVal { getCounterVal :: Int }
  deriving (Eq, Show, Generic)