{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExplicitForAll #-}

module ErrorHandling(
  ErrorHandler(..),
  JSONError(..),
  toHttpErr,
  jsonErr404,
  jsonErr409) where

import Servant
import Servant.API
import GHC.Generics
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive  (mk)
import Control.Monad.Trans.Except


data JSONError = JSONError
    { statusCode :: Int
    , title :: String
    , detail :: String
    } deriving (Show, Generic)

instance ToJSON JSONError

jsonErr404 :: JSONError
jsonErr404 = JSONError { statusCode = 404 , title = "", detail = "" }

jsonErr409 :: JSONError
jsonErr409 = JSONError { statusCode = 409 , title = "", detail = "" }

toHttpErr :: JSONError -> ServantErr
toHttpErr jsonError = err { errBody = jsonBody, errHeaders = [jsonHeader]}
  where
    err        = matchError . statusCode $ jsonError
    jsonBody   = encode jsonError
    jsonHeader = (mk $ pack "Content-Type",
                  pack "application/json;charset=utf-8")

matchError :: Int -> ServantErr
matchError 400 = err400
matchError 404 = err404
matchError 409 = err409

class (Monad m) => ErrorHandler e m | e -> m where
  convertErr :: e -> JSONError
  handleErr  :: ExceptT e m x -> Handler x
  {-# MINIMAL convertErr, handleErr #-}

