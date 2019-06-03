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

spec_add_user :: Spec
spec_add_user =
  describe "added user" $
  it "should be found" $ do
    cache <- users env
    _ <- addUser cache usr
    userByName cache "omd" >>= (\u -> u `shouldBe` Just usr)
  where
    env = makeEnv
    usr = User "omd" "globulon@gmail.com" (fromGregorian 1971 8 28)

spec_delete_user :: Spec
spec_delete_user =
  describe "delete user" $
  it "should delete user" $ do
    cache <- users env
    _ <- addUser cache usr
    _ <- dropUser cache "omd"
    userByName cache "omd" >>= (`shouldBe` Nothing)
  where
    env = makeEnv
    usr = User "omd" "globulon@gmail.com" (fromGregorian 1971 8 28)


