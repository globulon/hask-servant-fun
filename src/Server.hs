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
import Network.Wai.Handler.WarpTLS
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

getUsers :: Environment -> Handler [User]
getUsers = liftIO . allUsers

getUser :: Environment -> String -> Handler (Maybe User)
getUser env = liftIO . userByName env

postUser :: Environment-> User -> Handler ()
postUser env = liftIO . addUser env

deleteUser :: Environment -> String -> Handler ()
deleteUser env = liftIO . dropUser env

--boilerplate for phantom type (?)
api :: Proxy API
api = Proxy

server :: Environment -> Server API
server env = getUsers env :<|> getUser env :<|> postUser env :<|> deleteUser env

app :: Environment -> Application
app = serve api . server

startApp :: Environment -> IO ()
startApp = runTLS tlsOpts warpOpts . app
  where tlsOpts = tlsSettings "certs/cert.pem" "certs/key.pem"
        warpOpts = setPort 8080 defaultSettings


