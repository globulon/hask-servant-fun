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
import Environment

type API = "users" :> Get '[JSON] [User]
            :<|> "user" :> Capture "name" String :> Get '[JSON] (Maybe User)
            :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()

instance ToJSON User
instance FromJSON User

getUsers :: Environment -> Handler [User]
getUsers = liftIO . allUsers

getUser :: Environment -> String -> Handler (Maybe User)
getUser env = liftIO . userByName env

postUser :: Environment -> User -> Handler ()
postUser env = liftIO . addUser env

--boilerplate for phantom type (?)
api :: Proxy API
api = Proxy

server :: Environment -> Server API
server env = getUsers env :<|> getUser env :<|> postUser env

app :: Environment -> Application
app = serve api . server

startApp :: Environment -> IO ()
startApp = run 8080 . app