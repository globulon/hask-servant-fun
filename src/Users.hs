module Users(
    allUsers,
    userByName,
    addUser
  ) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup)
import qualified Data.Map as Map
import Environment(Users, Cached, Environment(..), makeEnv)
import Data.IORef

allUsers :: Environment -> IO [User]
allUsers env = users env >>= fmap Map.elems . readIORef

userByName :: Environment -> String ->  IO (Maybe User)
userByName env n = users env >>= fmap (Map.lookup n) . readIORef

addUser :: Environment -> User -> IO ()
addUser cached u@(User name  _ _) = users cached >>= (`modifyIORef` Map.insert name u)
