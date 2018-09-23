{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Maybe
import Network.Wai (Application, rawPathInfo, requestHeaderHost)
import Network.Wai.Application.Static (ssIndices, defaultWebAppSettings, staticApp)
import Network.Wai.UrlMap (mapUrls, mount, mountRoot)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Middleware.Vhost (vhost)
import WaiAppStatic.Types (toPieces)
import Debug.Trace
import qualified Data.ByteString as B

main :: IO ()
main = run 80 routerApp

routerApp :: Application
routerApp = vhost [{-(checkPrefix "gitlab", undefined)-}] mainPageApp
    where
        checkPrefix p = maybe False (B.isPrefixOf p) . requestHeaderHost


mainPageApp :: Application
mainPageApp = mapUrls $ mount "jump" jumpApp
                    <|> mountRoot errorHomeApp

errorHomeApp :: Application
errorHomeApp = staticAppWithDir "../error-home/home_dist"

jumpApp :: Application
jumpApp = staticAppWithDir "../error-home/jump_dist"

staticAppWithDir :: FilePath -> Application
staticAppWithDir path = staticApp $ settings {ssIndices = indices}
    where
        settings = defaultWebAppSettings path
        indices = fromJust $ toPieces ["index.html"]