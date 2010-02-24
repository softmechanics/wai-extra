{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Gzip
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic gzip compression of responses.
--
---------------------------------------------------------
module Network.Wai.Middleware.Gzip (gzip) where

import Network.Wai
import Network.Wai.Enumerator (fromLBS, toLBS)
import Codec.Compression.GZip (compress)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOneOf)
import qualified Data.ByteString.Char8 as B
import Control.Monad ((<=<))

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- Possible future enhancements:
--
-- * Only compress if the response is above a certain size.
--
-- * Add Content-Length.
--
-- * I read somewhere that \"the beast\" (MSIE) can\'t support compression
-- for Javascript files..
gzip :: Middleware
gzip app env = do
    res <- app env
    case responseBody res of
        Left _ -> return res
        Right _ -> do
            let enc = fromMaybe []
                    $ (splitOneOf "," . B.unpack)
                    `fmap` lookup AcceptEncoding
                      (requestHeaders env)
            if "gzip" `elem` enc
                then compressE (responseBody res) >>= \b -> return res
                    { responseBody = b
                    , responseHeaders = (ContentEncoding, B.pack "gzip")
                              : responseHeaders res
                    }
                else return res

compressE :: Either FilePath Enumerator -> IO (Either FilePath Enumerator)
compressE = either (return . Left) (fmap Right . compressE')

compressE' :: Enumerator -> IO Enumerator
compressE' = fromLBS . compress <=< toLBS
