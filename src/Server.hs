{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

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
import Domain
import Users

type UserAPI = "users" :> Get '[JSON] [User]
--            :<|> "user" :> Capture "name" String :> Get '[JSON] (Maybe User)

type API = UserAPI

data SortBy = Age | Name deriving (Eq, Show)

instance ToJSON User

getUsers :: Handler [User]
getUsers = return users

--getUser :: Maybe String -> Handler (Maybe User)
--getUser ms = ms >>= user

--boilerplate for phantom type (?)
api :: Proxy API
api = Proxy

server :: Server API
server = getUsers

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app