{-# LANGUAGE MultiParamTypeClasses #-}

module Algebras(UserRepo(..), SubsRepo(..)) where

import Domain(User(..), Subscription(..),)
import Environment(Environment(..), Users(..), Cached)
import Control.Monad.Trans.Reader  (ReaderT)
import Control.Monad.Except

class (Monad m) => UserRepo e m where
  allUsers :: ReaderT Environment  (ExceptT e m) [User]
  userByName :: String -> ReaderT Environment (ExceptT e m) User
  addUser :: User -> ReaderT Environment (ExceptT e m) ()
  dropUser :: String -> ReaderT Environment (ExceptT e m) ()
  {-# MINIMAL allUsers, userByName, addUser, dropUser #-}


class (Monad m) => SubsRepo e m where
  subsFor :: User -> ReaderT Environment (ExceptT e m) [Subscription]
  {-# MINIMAL subsFor #-}