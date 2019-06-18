{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Interpreters(UserRepo(..), UserError(..), UserIO, ErrorHandler(..)) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup, insert, delete)
import qualified Data.Map as Map
import Environment(Users, Cached, Environment(..), makeEnv)
import Data.IORef
import Algebras
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT, ask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except
import Data.Maybe
import GHC.Generics
import ErrorHandling(ErrorHandler(..), JSONError(..), toHttpErr, jsonErr404, jsonErr409)
import Servant
import Servant.API

data UserError = UserNotFound String | UserConflict String deriving (Eq, Show, Generic)

type UserIO = ReaderT Environment (ExceptT UserError IO)

instance UserRepo UserError IO where
  allUsers = do
    Environment { users = us } <- ask
    liftIO (fmap elems . readIORef $ us)

  userByName s = do
    Environment { users = us } <- ask
    lift ( ExceptT (fmap ( toErr . Map.lookup s) . readIORef $ us) )
    where
      toErr = maybe (Left . UserNotFound $ s) Right

  addUser u@User { name = n }  = do
    Environment { users = us } <- ask
    liftIO (modifyIORef' us (Map.insert n u))

  dropUser n = do
    Environment { users = us } <- ask
    liftIO (modifyIORef' us (Map.delete n))


instance ErrorHandler UserError IO where
  convertErr (UserNotFound s) = jsonErr404 { title = "Missing User" , detail = s }
  convertErr (UserConflict s) = jsonErr409 { title = "Existing User" , detail = s }
  handleErr x                 = Handler { runHandler' = withExceptT (toHttpErr . convertErr) x }
