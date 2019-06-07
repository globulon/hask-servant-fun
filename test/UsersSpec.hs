module UsersSpec where

import Domain(User(..))
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Time.Calendar
import Environment
import Algebras
import Interpreters


spec :: Spec
spec = do
  spec_add_user
  spec_delete_user
  spec_all_users

spec_add_user :: Spec
spec_add_user =
  describe "added user" $
  it "should be found" $ do
    cache <- env
    _ <- addUser cache usr
    userByName cache "omd" >>= (`shouldBe` Just usr)
  where
    env = makeEnv

spec_delete_user :: Spec
spec_delete_user =
  describe "delete user" $
  it "should delete user" $ do
    cache <- env
    _ <- addUser cache usr
    _ <- dropUser cache "omd"
    userByName cache "omd" >>= (`shouldBe` Nothing)
  where
    env = makeEnv


spec_all_users :: Spec
spec_all_users =
  describe "all users" $ do
    it "should send no users" $
      makeEnv >>= allUsers >>= (`shouldBe` [])
    it "should send a created user" $ do
      e <- makeEnv
      _ <- addUser e usr
      userByName e "omd" >>= (`shouldBe` Just usr)


usr :: User
usr = User "omd" "globulon@gmail.com" (fromGregorian 1971 8 28)
