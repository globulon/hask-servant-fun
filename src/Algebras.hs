{-# LANGUAGE MultiParamTypeClasses #-}

module Algebras(UserRepo(..)) where

import Domain(User(..))
import Environment(Environment(..), Users(..), Cached)
import Control.Monad.Trans.Reader  (ReaderT)
import Control.Monad.Except

class (Monad m) => UserRepo e m where
  allUsers :: ReaderT Environment  (ExceptT e m) [User]
  userByName :: String -> ReaderT Environment (ExceptT e m) User
  addUser :: User -> ReaderT Environment (ExceptT e m) ()
  dropUser :: String -> ReaderT Environment (ExceptT e m) ()
  {-# MINIMAL allUsers, userByName, addUser, dropUser #-}