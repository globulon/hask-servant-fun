{-# LANGUAGE ScopedTypeVariables #-}
module UsersSpec where

import Domain(User(..))
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Time.Calendar
import Environment
import Algebras
import Interpreters
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT, ask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except

spec :: Spec
spec = do
  spec_add_user
  spec_delete_user
  spec_all_users

spec_add_user :: Spec
spec_add_user =
  describe "added user" $
  it "should be found" $
    let u :: UserIO User = do
          _ <- addUser usr
          userByName (name usr)
    in makeEnv >>= runExceptT . runReaderT u >>= (`shouldBe` Right usr)

spec_delete_user :: Spec
spec_delete_user =
  describe "delete user" $
  it "should delete user" $
    let u :: UserIO User = do
            n <- fmap name (return usr)
            _ <- addUser usr
            _ <- dropUser n
            userByName n
    in makeEnv >>= runExceptT . runReaderT u >>= (`shouldBe` Left (UserNotFound "omd"))

spec_all_users :: Spec
spec_all_users =
  describe "all users" $ do
    it "should send no users" $
      makeEnv >>= runExceptT . runReaderT (allUsers :: UserIO [User]) >>= (`shouldBe` Right [])
    it "should send a created user" $
      let us :: UserIO [User] = do
               _ <- addUser usr
               allUsers
      in makeEnv >>= runExceptT . runReaderT us >>= (`shouldBe` Right [usr])

usr :: User
usr = User "omd" "globulon@gmail.com" (fromGregorian 1971 8 28)
