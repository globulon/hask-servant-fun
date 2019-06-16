module Algebras(UserRepo(..)) where

import Domain(User(..))
import Environment(Environment(..), Users(..), Cached)
import Control.Monad.Trans.Reader  (ReaderT)


class UserRepo m where
  allUsers :: ReaderT Environment  m [User]
  userByName :: String -> ReaderT Environment  m (Maybe User)
  addUser :: User -> ReaderT Environment m ()
  dropUser :: String -> ReaderT Environment m ()
  {-# MINIMAL allUsers, userByName, addUser, dropUser #-}