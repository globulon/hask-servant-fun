module Users(
    allUsers,
    userByName
  ) where

import Data.Time.Calendar
import Domain(User(..))
import Data.Map (Map, elems, fromList, lookup)
import qualified Data.Map as Map
import Data.IORef

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

cached :: IO (IORef (Map String User))
cached = newIORef (Map.fromList [("isaac",isaac), ("albert", albert)])

allUsers :: IO [User]
allUsers = cached >>= fmap Map.elems . readIORef

userByName :: String -> IO (Maybe User)
userByName n = cached >>= fmap (Map.lookup n) . readIORef

