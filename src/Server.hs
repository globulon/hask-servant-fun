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
import Control.Monad.IO.Class (liftIO, MonadIO)

type API = "users" :> Get '[JSON] [User]
            :<|> "user" :> Capture "name" String :> Get '[JSON] (Maybe User)
            :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User

instance ToJSON User
instance FromJSON User

getUsers :: Handler [User]
getUsers = liftIO allUsers

getUser :: String -> Handler (Maybe User)
getUser = liftIO . userByName

postUser :: User -> Handler User
postUser = liftIO . addUser

--boilerplate for phantom type (?)
api :: Proxy API
api = Proxy

server :: Server API
server = getUsers :<|> getUser :<|> postUser

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app