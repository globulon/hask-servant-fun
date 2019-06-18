{-# LANGUAGE DeriveGeneric #-}

module Http where

import Servant
import Servant.API
import GHC.Generics
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive  (mk)

data JSONError = JSONError
    { statusCode :: Int
    , title :: String
    , detail :: String
    } deriving (Show, Generic)

instance ToJSON JSONError

jsonErr404 :: JSONError
jsonErr404 = JSONError { statusCode = 404 , title = "", detail = "" }

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