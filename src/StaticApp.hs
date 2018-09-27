{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module StaticApp
    ( staticAppWithAllowedRequests
    ) where

import Control.Monad.IO.Class
import Data.Maybe
import Network.HTTP.Date (parseHTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import Network.HTTP.Types
import Network.Mime (MimeType)
import Network.Wai 
import Network.Wai.Application.Static (defaultWebAppSettings)
import WaiAppStatic.Types 
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB 
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


staticAppWithAllowedRequests :: FilePath -> [Request -> Bool] -> Application
staticAppWithAllowedRequests path ars req = staticAppPieces (settings {ssIndices = indices}) (pathInfo req) ars req
    where
        settings = defaultWebAppSettings path
        indices = fromJust $ toPieces ["index.html"]

staticAppPieces :: StaticSettings -> [T.Text] -> [Request -> Bool] -> Application
staticAppPieces ss rawPieces ars req sendResponse
    | requestMethod req `notElem`["GET", "HEAD"] = sendResponse $ responseLBS status405 [("Content-Type", "text/plain")] "Only GET or HEAD is supported"
    | any (\f -> f req) ars = liftIO $ checkPieces ss (map unsafeToPiece rawPieces) req >>= response
    | otherwise = liftIO . maybe forbidden resp $ toPieces rawPieces 
    where
        forbidden = sendResponse $ responseLBS status403 [("Content-Type", "text/plain")] "Forbidden"
        resp pieces = checkPieces ss pieces req >>= response

        response :: StaticResponse -> IO ResponseReceived
        response (FileResponse file ch) = ssGetMimeType ss file >>= sendResponse . fileToResponse file status200 . headers
            where
                filesize = fileGetSize file
                headers mimetype = ("Content-Type", mimetype) : ch

        response NotModified = sendResponse $ responseLBS status304 [] ""

        response (SendContent mt lbs) = sendResponse $ responseLBS status200 [("Content-Type", mt)] lbs
 
        response (Redirect pieces' mHash) = 
            sendResponse $ responseLBS status301
                [ ("Content-Type", "text/plain")
                , ("Location", B8.append loc $ renderQuery True qString)
                ] "Redirect"
            where
                loc = ssMkRedirect ss pieces' $ BL.toStrict $ BB.toLazyByteString (encodePathSegments $ map fromPiece pieces')
                qString = maybe (remove "etag" $ queryString req) (\hash -> replace "etag" (Just hash) $ queryString req) mHash

        response (RawRedirect path) = sendResponse $ responseLBS status301
                [ ("Content-Type", "text/plain")
                , ("Location", path)
                ] "Redirect"

        response NotFound = maybe notfound resp $ ss404Handler ss
            where
                notfound = sendResponse $ responseLBS status404 [("Content-Type", "text/plain")] "File not found"
                resp app = app req sendResponse

        response (WaiResponse r) = sendResponse r

data StaticResponse =
    Redirect Pieces (Maybe B.ByteString)
    | RawRedirect B.ByteString
    | NotFound
    | FileResponse File ResponseHeaders
    | NotModified
    | SendContent MimeType BL.ByteString
    | WaiResponse Response

safeInit  :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

filterButLast :: (a -> Bool) -> [a] -> [a]
filterButLast f (x : y : zs)
    | f x = x : filterButLast f (y : zs)
    | otherwise = filterButLast f (y : zs)
filterButLast _ xs = xs

serveFolder :: StaticSettings -> Pieces -> Request -> Folder -> IO StaticResponse
serveFolder StaticSettings {..} pieces req folder = checkListing ssListing
    where
        checkListing (Just listing)
            | Just path <- addTrailingSlash req, ssAddTrailingSlash = return $ RawRedirect path
            | otherwise = resp <$> listing pieces folder
            where
                resp = WaiResponse . responseBuilder status200 [("Content-Type", "text/html; charset=utf-8")]
        checkListing _ = return . WaiResponse $ responseLBS status403 [("Content-Type", "text/plain")] "Directory listings disabled"

addTrailingSlash :: Request -> Maybe B.ByteString
addTrailingSlash req
    | B8.null rp = Just "/"
    | B8.last rp == '/' = Nothing
    | otherwise = Just $ B8.snoc rp '/'
  where
    rp = rawPathInfo req

replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replcatek k v (x : xs)
    | fst x == k = (k, v) : xs
    | otherwise = x : replace k v xs
replace k v _ = [(k, v)]

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove k (x : xs)
    | fst x == k = xs
    | otherwise = x : remove k xs
remove _ _ = []

dropLastIfNull :: [Piece] -> [Piece]
dropLastIfNull [fromPiece -> ""] = []
dropLastIfNull (x : xs) = x : dropLastIfNull xs
dropLastIfNull _ = []

checkPieces :: StaticSettings -> Pieces -> Request -> IO StaticResponse
checkPieces ss@StaticSettings {..} pieces req
    | any (T.null . fromPiece) $ safeInit pieces = return $ Redirect (filterButLast (not . T.null . fromPiece) pieces) Nothing
    | otherwise = lookupResult >>= checkRes
  where
    checkRes (Left location) = return $ RawRedirect location
    checkRes (Right LRNotFound) = return NotFound
    checkRes (Right (LRFile file)) = serveFile ss req file
    checkRes (Right (LRFolder folder)) = serveFolder ss pieces req folder

    lookupResult :: IO (Either B.ByteString LookupResult)
    lookupResult = ssLookupFile pieces >>= checkNonIndexRes
        where 
            checkNonIndexRes res@LRFile{} = return $ Right res
            checkNonIndexRes nonIndexResult = checkEIndexRes <$> lookupIndices (map (\ index -> dropLastIfNull pieces ++ [index]) ssIndices)
                where 
                    checkEIndexRes (Left redirect) = Left redirect
                    checkEIndexRes (Right indexResult) = checkIndexResult indexResult
                        where 
                            checkIndexResult LRNotFound = Right nonIndexResult
                            checkIndexResult (LRFile file)
                                | ssRedirectToIndex = Left $ TE.encodeUtf8 relPath
                                where 
                                    relPath = makeRelPath $ reverse pieces
                                    makeRelPath [] = fromPiece $ fileName file
                                    makeRelPath (lastSegment : _) = checkLastSeg $ fromPiece lastSegment
                                        where
                                            checkLastSeg "" = fromPiece $ fileName file
                                            checkLastSeg lastSegment' = T.concat
                                                                        [ lastSegment'
                                                                        , "/"
                                                                        , fromPiece $ fileName file
                                                                        ]
                            checkIndexResult _ = Right indexResult

    lookupIndices :: [Pieces] -> IO (Either B.ByteString LookupResult)
    lookupIndices (x : xs) = ssLookupFile x >>= checkRes
        where 
            checkRes LRNotFound = lookupIndices xs
            checkRes res
                | ssAddTrailingSlash, Just redirect <- addTrailingSlash req = return $ Left redirect
                | otherwise = return $ Right res
    lookupIndices [] = return $ Right LRNotFound

--
serveFile :: StaticSettings -> Request -> File -> IO StaticResponse
serveFile StaticSettings {..} req file
    | ssUseHash = fileGetHash file >>= checkHash . (, lookup "if-none-match" $ requestHeaders req)
    | otherwise = lastMod
  where
    checkHash (Just hash, Just lastHash)
        | hash == lastHash = return NotModified
    checkHash (Just hash, _) = respond [("ETag", hash)]
    checkHash _ = lastMod

    mLastSent = lookup "if-modified-since" (requestHeaders req) >>= parseHTTPDate
    lastMod = makeLastMod (fmap epochTimeToHTTPDate $ fileGetModified file, mLastSent)

    makeLastMod (Just mdate, Just lastSent)
        | mdate == lastSent = return NotModified
    makeLastMod (Just mdate, _) = respond [("last-modified", formatHTTPDate mdate)]
    makeLastMod _ = respond []

    respond headers = return . FileResponse file $ cacheControl ssMaxAge headers

cacheControl :: MaxAge -> (ResponseHeaders -> ResponseHeaders)
cacheControl maxage = headerCacheControl . headerExpires
  where
    ccInt = makeCcInt maxage

    makeCcInt NoMaxAge = Nothing
    makeCcInt (MaxAgeSeconds i) = Just i
    makeCcInt MaxAgeForever = Just oneYear
    
    oneYear :: Int
    oneYear = 60 * 60 * 24 * 365

    headerCacheControl = checkCcInt ccInt
        where 
            checkCcInt (Just i) = (:) ("Cache-Control", B8.append "public, max-age=" $ B8.pack $ show i)
            checkCcInt _ = id

    headerExpires = checkMaxAge maxage
        where 
            checkMaxAge MaxAgeForever = (:) ("Expires", "Thu, 31 Dec 2037 23:55:55 GMT")
            checkMaxAge _ = id

