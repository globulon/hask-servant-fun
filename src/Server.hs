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
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)

type API = "users" :> Get '[JSON] [User]
            :<|> "user" :> Capture "name" String :> Get '[JSON] (Maybe User)
            :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()
            :<|> "user" :> Capture "name" String :> Delete '[JSON] ()

instance ToJSON User
instance FromJSON User

type AppM = ReaderT Environment Handler

server :: ServerT API AppM
server = allUsers :<|> userByName :<|> addUser :<|> dropUser

nt :: Environment -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Environment -> Application
app env = serve api $ hoistServer api (nt env) server
  where api = Proxy :: Proxy API

startApp :: Environment -> IO ()
startApp = runTLS tlsOpts warpOpts . app
  where tlsOpts = tlsSettings "certs/cert.pem" "certs/key.pem"
        warpOpts = setPort 8080 defaultSettings
