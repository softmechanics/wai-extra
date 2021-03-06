{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.CleanPath
    ( cleanPath
    , cleanPathRel
    , cleanPathFunc
    , splitPath
    ) where

import Network.Wai
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network.URI (unEscapeString)
import qualified Data.ByteString.UTF8 as BSU

cleanPathFunc :: (B.ByteString -> Either B.ByteString [String])
              -> B.ByteString
              -> ([String] -> Request -> IO Response)
              -> Request
              -> IO Response
cleanPathFunc splitter prefix app env =
    case splitter $ pathInfo env of
        Right pieces -> app pieces env
        Left p -> return
                . Response status301
                  [("Location", B.concat [prefix, p, suffix])]
                $ ResponseLBS L.empty
    where
        -- include the query string if present
        suffix =
            case B.uncons $ queryString env of
                Nothing -> B.empty
                Just ('?', _) -> queryString env
                _ -> B.cons '?' $ queryString env

-- | Performs redirects as per 'splitPath'.
cleanPathRel :: B.ByteString -> ([String] -> Request -> IO Response)
             -> Request -> IO Response
cleanPathRel = cleanPathFunc splitPath

cleanPath :: ([String] -> Request -> IO Response) -> Request -> IO Response
cleanPath = cleanPathRel B.empty

-- | Given a certain requested path, return either a corrected path
-- to redirect to or the tokenized path.
--
-- This code corrects for the following issues:
--
-- * It is missing a trailing slash, and there is no period after the
-- last slash.
--
-- * There are any doubled slashes.
splitPath :: B.ByteString -> Either B.ByteString [String]
splitPath s =
    let corrected = B.pack $ rts $ ats $ rds $ B.unpack s
     in if corrected == s
            then Right $ map (BSU.toString . B.pack . unEscapeString . B.unpack)
                       $ filter (not . B.null)
                       $ B.split '/' s
            else Left corrected

-- | Remove double slashes
rds :: String -> String
rds [] = []
rds [x] = [x]
rds (a:b:c)
    | a == '/' && b == '/' = rds (b:c)
    | otherwise = a : rds (b:c)

-- | Add a trailing slash if it is missing. Empty string is left alone.
ats :: String -> String
ats [] = []
ats s =
    if last s == '/' || dbs (reverse s)
        then s
        else s ++ "/"

-- | Remove a trailing slash if the last piece has a period.
rts :: String -> String
rts [] = []
rts s =
    if last s == '/' && dbs (tail $ reverse s)
        then init s
        else s

-- | Is there a period before a slash here?
dbs :: String -> Bool
dbs ('/':_) = False
dbs ('.':_) = True
dbs (_:x) = dbs x
dbs [] = False
