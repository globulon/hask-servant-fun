module Algebras(UserRepo(..)) where

import Domain(User(..))
import Environment(Environment(..), Users(..))

class UserRepo m where
  allUsers :: Environment -> m [User]
  userByName :: Environment -> String ->  m (Maybe User)
  addUser :: Environment -> User -> m ()
  {-# MINIMAL allUsers, userByName, addUser #-}