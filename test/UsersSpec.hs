module UsersSpec where

import Users
import Domain(User(..))
import Test.Hspec
import Test.QuickCheck
import Control.Monad

spec :: Spec
spec =
  describe "user by name" $ do
    it "should not deliver unknown user" $
      userByName >=> (`shouldBe` Nothing) $ "unknown"
    it "should deliver known user" $
      userByName  >=> (\n -> fmap name n `shouldBe` Just "Isaac Newton") $ "isaac"