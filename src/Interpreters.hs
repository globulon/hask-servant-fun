module Interpreters(UserRepo(..)) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup, insert, delete)
import qualified Data.Map as Map
import Environment(Users, Cached, Environment(..), makeEnv)
import Data.IORef
import Algebras
import Control.Monad.Trans.Reader  (ReaderT, ask)
import Control.Monad.Trans (lift)
import Servant
import Servant.API
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)


instance UserRepo IO where
  allUsers = do
    Environment { users = us } <- ask
    lift (fmap elems . readIORef $ us)

  userByName s = do
    Environment { users = us } <- ask
    lift (fmap (Map.lookup s) . readIORef $ us)

  addUser u@User { name = n }  = do
      Environment { users = us } <- ask
      lift (modifyIORef' us (Map.insert n u))

  dropUser n = do
    Environment { users = us } <- ask
    lift (modifyIORef' us (Map.delete n))


instance UserRepo Handler where
  allUsers = mapReaderT liftIO allUsers
  userByName = mapReaderT liftIO . userByName
  addUser = mapReaderT liftIO . addUser
  dropUser = mapReaderT liftIO . dropUser
