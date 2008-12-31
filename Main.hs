{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad.Trans
import System.Environment

import HAppS.Server
import LastFm

-- TODO
-- If there's a token, use it to get a session key, set a cookie
-- If there's no token, read a cookie with the session key, do the handshake
-- If all fails, redirect to authentication
main :: IO ()
main = do
    (key : secret : []) <- getArgs 
    simpleHTTP (nullConf { port = 8016 })
        [ dir "key"       [ anyRequest $ ok . toResponse $ key ]
        , dir "handshake" [ withData (\token -> [anyRequest $ shakeHands key secret token]) ]
        , dir "proxy"     [ proxyServe ["*.audioscrobbler.com:80"] ]
        , fileServe ["index.html"] "static"
        ] 

-- Parsing submitted data

instance FromData Token where
    fromData = look "token"

shakeHands :: Key -> Secret -> Token -> Web Response
shakeHands key secret token = do
    session <- liftIO $ getSession key secret token
    case session of
        Left err -> jsonError err
        Right s  -> do
            handshake <- liftIO $ getHandshake key secret s
            case handshake of
                Left err                         -> jsonError err
                Right (Handshake key npurl surl) -> ok . toResponse $ "{\"handshake\":" ++ show [key, npurl, surl] ++ "}"

jsonError :: (Show a) => a -> Web Response
jsonError e = badRequest . toResponse $ "{\"error\":" ++ show (show e) ++ "}"
