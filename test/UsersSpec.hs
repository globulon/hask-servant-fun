module UsersSpec where

import Users
import Domain(User(..))
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Time.Calendar
import Environment(makeEnv)

spec :: Spec
spec = spec_add_user

spec_add_user :: Spec
spec_add_user =
  describe "added user" $
    it "should be found" $
      addUser env >=>
        (\_ -> userByName env "omd") >=>
          return . fmap name >=> (`shouldBe` Just "omd") $
            (User "omd" "globulon@gmail.com" (fromGregorian 1971 08 28))
            where
              env = makeEnv

