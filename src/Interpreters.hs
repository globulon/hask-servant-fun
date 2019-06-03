module Interpreters(UserRepo(..)) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup)
import qualified Data.Map as Map
import Environment(Users, Cached, Environment(..), makeEnv)
import Data.IORef
import Algebras

instance UserRepo IO where
  allUsers = fmap elems . readIORef
  userByName env n = fmap (Map.lookup n) . readIORef $ env
  addUser env u@(User name  _ _) = modifyIORef' env (Map.insert name u)
