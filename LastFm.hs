module LastFm where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Data.List
import Data.Digest.OpenSSL.MD5

import System.Time
import System.IO

import qualified Text.JSON as J
import Text.JSON

import Network.URI
import Network.HTTP
import qualified Network.Stream as N
import Network.Stream

-- Configuration

api_uri, post_uri :: URI
api_uri    = URI "http:" (Just $ URIAuth "" "ws.audioscrobbler.com" "") "/2.0/?format=json&" "" ""
post_uri   = URI "http:" (Just $ URIAuth "" "post.audioscrobbler.com" "") "/?" "" ""

-- Types

type Error  = String
type User   = String
type Key    = String
type Secret = String
type Token  = String
type Params = M.Map String String

data Session = Session User Key deriving Show
data Handshake = Handshake { session_key    :: Key
                           , np_url         :: String
                           , submission_url :: String
                           } deriving (Show)

data ClientConf = ClientConf { api_key        :: Key
                             , api_secret     :: Secret
                             , client_id      :: String
                             , client_version :: String
                             }

-- JSON

instance JSON Session where
    readJSON (JSObject obj) =
        lookupObj "session" obj >>=
            (\obj' -> Session <$> lookupObj "name" obj' <*> lookupObj "key" obj') 
    showJSON = undefined

lookupObj :: (JSON a) => String -> JSObject JSValue -> J.Result a
lookupObj key = maybe (Error $ "Key '" ++ key ++ "' not found in object") readJSON
              . lookup key 
              . fromJSObject

decode' :: (JSON a) => String -> N.Result a
decode' = convertResult . decode
    where convertResult (Ok a)    = Right a
          convertResult (Error e) = Left $ ErrorMisc e

-- Session

getSession :: ClientConf -> Token -> IO (N.Result Session)
getSession conf token = do 
    result <- simpleHTTP $ Request (api_uri { uriQuery = sessionRequest conf token }) GET [] ""
    case result of
        (Left  err)      -> return $ Left err
        (Right response) -> do
            case rspCode response of
                -- (2,0,0) -> return $ Right (rspBody response)
                (2,0,0) -> return $ decode' (rspBody response)
                _       -> return $ Left (ErrorMisc $ "Getting session failed" ++ rspBody response)


sessionRequest :: ClientConf -> Token -> String
sessionRequest conf token = flattenParams $ M.insert "api_sig" (signRequest request (api_secret conf)) request
    where (*) = (,)
          request = M.fromList 
                    [ "method"  * "auth.getSession"
                    , "token"   * token
                    , "api_key" * api_key conf
                    ]

signRequest :: Params -> Secret -> String
signRequest params secret = md5sum $ U.fromString $ concatMap (\(k, v) -> k ++ v) (M.toAscList params) ++ secret

-- Handshake

getHandshake :: ClientConf -> Session -> IO (N.Result Handshake)
getHandshake conf session = do
    query  <- handshakeQuery conf session
    result <- simpleHTTP $ Request (post_uri { uriQuery = query }) GET [] ""
    case result of
        (Left  err)      -> return $ Left err
        (Right response) -> 
            case rspCode response of
                -- (2,0,0) -> return $ Right (rspBody response)
                (2,0,0) -> return $ parseHandshake $ rspBody response
                _       -> return $ Left (ErrorMisc $ "Getting handshake failed: " ++ rspBody response)


handshakeQuery :: ClientConf -> Session -> IO String
handshakeQuery conf (Session username session_key) = do
    timestamp <- liftM (\(TOD unix _) -> show unix) getClockTime
    return $ flattenParams $ M.fromList -- Uhg
        [ "hs"         * "true"
        , "p"          * "1.2.1"
        , "c"          * client_id conf
        , "v"          * client_version conf
        , "u"          * username
        , "t"          * timestamp
        , "a"          * md5sum (U.fromString $ (api_secret conf) ++ timestamp)
        , "api_key"    * api_key conf
        , "sk"         * session_key
        ]
    where (*) = (,)

parseHandshake :: String -> N.Result Handshake
parseHandshake response = case lines response of
    ["OK", key, npurl, surl] -> Right $ Handshake key npurl surl
    err                      -> Left  $ ErrorMisc ("Handshake failed: " ++ (unlines err))

-- Utils

flattenParams :: Params -> String
flattenParams = concat . intersperse "&" . map (\(k, v) -> k ++ '=' :  v) . M.toList
