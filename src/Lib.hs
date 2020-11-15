{-# LANGUAGE OverloadedStrings #-}
module Lib ( uploadRequest ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Client.MultipartFormData

uploadRequest :: FilePath -> IO String
uploadRequest file = do
  req <- (formDataBody [partFile "file" file] $
         (parseRequest_ "https://lainsafe.delegao.moe/upload.cgi") {method="POST"
                                                                   , requestHeaders = [("Content-Type", "multipart/form-data")]})
  man <- newManager tlsManagerSettings
  response <- httpLbs req man
  return . L8.unpack $ responseBody response
