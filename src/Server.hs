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
import Interpreters(UserRepo(..), UserError(..), UserIO, ErrorHandler(..))
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Trans.Except
import ErrorHandling

instance ToJSON User
instance FromJSON User
instance ToJSON Subscription

type UserAPI = "users" :> Get '[JSON] [User]
            :<|> "user" :> Capture "name" String :> Get '[JSON] User
            :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()
            :<|> "user" :> Capture "name" String :> Delete '[JSON] ()

type API = UserAPI

server :: ServerT API UserIO
server = allUsers :<|> userByName :<|> addUser :<|> dropUser

transform :: Environment -> UserIO a -> Handler a
transform env x = handleErr ( runReaderT x env )

app :: Environment -> Application
app env = serve api . hoistServer api (transform env) $ server
  where api = Proxy :: Proxy API

startApp :: Environment -> IO ()
startApp = runTLS tlsOpts warpOpts . app
  where tlsOpts = tlsSettings "certs/cert.pem" "certs/key.pem"
        warpOpts = setPort 8080 defaultSettings
