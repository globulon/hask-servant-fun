module Interpreters(UserRepo(..)) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup)
import qualified Data.Map as Map
import Environment(Users, Cached, Environment(..), makeEnv)
import Data.IORef
import Algebras

instance UserRepo IO where
  allUsers env = users env >>= fmap Map.elems . readIORef
  userByName env n = users env >>= fmap (Map.lookup n) . readIORef
  addUser cached u@(User name  _ _) = users cached >>= (`modifyIORef` Map.insert name u)
