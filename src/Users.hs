module Users(
    users,
    user
  ) where

import Data.Time.Calendar
import Domain
import Data.Map (Map, elems, fromList, lookup)
import qualified Data.Map as Map

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

cached :: Map String User
cached = Map.fromList [("issac",isaac), ("albert", albert)]

users :: [User]
users = Map.elems cached

user :: String -> Maybe User
user n = Map.lookup n cached

