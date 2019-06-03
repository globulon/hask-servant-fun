module Algebras(UserRepo(..)) where

import Domain(User(..))
import Environment(Environment(..), Users(..), Cached)

class UserRepo m where
  allUsers :: Cached Users -> m [User]
  userByName :: Cached Users -> String ->  m (Maybe User)
  addUser :: Cached Users -> User -> m ()
  {-# MINIMAL allUsers, userByName, addUser #-}