{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}

module Server(startApp, app) where

import GHC.Generics
import Data.Text
import Data.Time.Calendar
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API


type API = "users" :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

server :: Server API
server = return users


api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app