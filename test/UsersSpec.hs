module UsersSpec where

import Users
import Domain(User(..))
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Time.Calendar

spec :: Spec
spec = do
  spec_user_by_name
  spec_add_user

spec_user_by_name :: Spec
spec_user_by_name =
  describe "user by name" $ do
    it "should not deliver unknown user" $
      userByName >=> (`shouldBe` Nothing) $ "unknown"
    it "should deliver known user" $
      userByName >=> (return . fmap name) >=> (`shouldBe` Just "Isaac Newton") $ "isaac"

spec_add_user :: Spec
spec_add_user =
  describe "added user" $
    it "should be found" $
      addUser >=>
        (\_ -> userByName "omd") >=>
          return . fmap name >=> (`shouldBe` Just "omd") $
            (User "omd" 47 "globulon@gmail.com" (fromGregorian 1071 08 28))