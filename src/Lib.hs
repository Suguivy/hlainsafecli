{-# LANGUAGE OverloadedStrings #-}
module Lib ( uploadRequest ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Client.MultipartFormData

type Url = String

uploadRequest :: Url -> FilePath -> IO String
uploadRequest url file = do
  req <- (formDataBody [partFile "file" file] $
         (parseRequest_ url) {method="POST"
                             , requestHeaders = [("Content-Type", "multipart/form-data")]})
  man <- newManager tlsManagerSettings
  response <- httpLbs req man
  return . L8.unpack $ responseBody response
