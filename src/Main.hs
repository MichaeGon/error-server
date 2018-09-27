{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import StaticApp (staticAppWithAllowedRequests)

import Control.Concurrent.Async (concurrently)
import Data.Maybe 
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.ReverseProxy (ProxyDest(..), WaiProxyResponse(..), waiProxyTo, defaultOnExc)
import Network.Wai (Application, Request, requestHeaderHost, pathInfo)
import Network.Wai.Handler.Warp (Port, run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import qualified Data.ByteString as B

main :: IO ()
main = () <$ concurrently appHTTP appHTTPS
    where
        appHTTP = newManager defaultManagerSettings >>= run httpPort . forceSSL . waiProxyTo redirectFunc defaultOnExc
        appHTTPS = newTlsManager >>= runTLS tls (setPort httpsPort defaultSettings) . waiProxyTo reverseFunc defaultOnExc
        tls = tlsSettings certFile keyFile

httpPort :: Port
httpPort = 80

httpsPort :: Port
httpsPort = 443

certFile :: FilePath
certFile = "certFilePath"

keyFile :: FilePath
keyFile = "keyFilePath"

rootDir :: FilePath
rootDir = "rootDirPath"

redirectFunc :: Request -> IO WaiProxyResponse
redirectFunc req = return $ WPRProxyDest $ ProxyDest {pdHost = fromJust $ requestHeaderHost req, pdPort = 443}

reverseFunc :: Request -> IO WaiProxyResponse
reverseFunc req
    | isGitLab req = return . WPRProxyDest $ ProxyDest {pdHost = fromJust $ requestHeaderHost req, pdPort = 7000}
    | otherwise = return $ WPRApplication errorHomeApp

isChallenge :: Request -> Bool
isChallenge = (== [".well-known", "acme-challenge"]) . take 2 . pathInfo

isGitLab :: Request -> Bool
isGitLab = checkPrefix "gitlab" 

checkPrefix :: B.ByteString -> Request -> Bool
checkPrefix p = maybe False (B.isPrefixOf p) . requestHeaderHost

errorHomeApp :: Application
errorHomeApp = staticAppWithAllowedRequests rootDir [isChallenge]
