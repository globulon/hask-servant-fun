module Interpreters(UserRepo(..)) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup, insert, delete)
import qualified Data.Map as Map
import Environment(Users, Cached, Environment(..), makeEnv)
import Data.IORef
import Algebras

instance UserRepo IO where
  allUsers = fmap elems . readIORef . users
  userByName env n = fmap (Map.lookup n) . readIORef . users $ env
  addUser env u@(User n  _ _) = modifyIORef' (users env) (Map.insert n u)
  dropUser env n = modifyIORef' (users env) (Map.delete n)
