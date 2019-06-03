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
import Control.Monad.IO.Class (liftIO, MonadIO)
import Environment
import Algebras
import Interpreters

type API = "users" :> Get '[JSON] [User]
            :<|> "user" :> Capture "name" String :> Get '[JSON] (Maybe User)
            :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()
            :<|> "user" :> Capture "name" String :> Delete '[JSON] ()

instance ToJSON User
instance FromJSON User

getUsers :: Cached Users -> Handler [User]
getUsers = liftIO . allUsers

getUser :: Cached Users -> String -> Handler (Maybe User)
getUser env = liftIO . userByName env

postUser :: Cached Users -> User -> Handler ()
postUser env = liftIO . addUser env

deleteUser :: Cached Users -> String -> Handler ()
deleteUser env = liftIO . dropUser env

--boilerplate for phantom type (?)
api :: Proxy API
api = Proxy

server :: Cached Users -> Server API
server env = getUsers env :<|> getUser env :<|> postUser env :<|> deleteUser env

app :: Cached Users -> Application
app = serve api . server

startApp :: Cached Users -> IO ()
startApp = run 8080 . app