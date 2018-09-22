{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import WaiAppStatic.Types

main :: IO ()
main = run 3000 errorHomeApp

errorHomeApp :: Application
errorHomeApp = staticApp $ settings {ssIndices = indices}
    where
        settings = defaultWebAppSettings "../error-home/contents"
        indices = fromJust $ toPieces ["index.html"]