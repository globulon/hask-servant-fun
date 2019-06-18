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
import Interpreters(UserRepo(..), UserError(..), UserIO)
import Control.Monad.Trans.Reader (ReaderT(..), mapReaderT)
import Control.Monad.Trans.Except
import Http

type API = "users" :> Get '[JSON] [User]
            :<|> "user" :> Capture "name" String :> Get '[JSON] User
            :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()
            :<|> "user" :> Capture "name" String :> Delete '[JSON] ()

instance ToJSON User
instance FromJSON User
instance ToJSON UserError

server :: ServerT API UserIO
server = allUsers :<|> userByName :<|> addUser :<|> dropUser

-- ExceptT ServantErr IO

nt :: Environment -> UserIO a -> Handler a
nt env x =   Handler { runHandler' = withExceptT convertUserErr (runReaderT x env) }

convertUserErr :: UserError -> ServantErr
convertUserErr (UserNotFound s) = toHttpErr jsonErr404 { title = "Missing User" , detail = s }
convertUserErr (UserConflict s) = toHttpErr jsonErr404 { title = "Existing User" , detail = s }

app :: Environment -> Application
app env = serve api $ hoistServer api (nt env) server
  where api = Proxy :: Proxy API

startApp :: Environment -> IO ()
startApp = runTLS tlsOpts warpOpts . app
  where tlsOpts = tlsSettings "certs/cert.pem" "certs/key.pem"
        warpOpts = setPort 8080 defaultSettings
