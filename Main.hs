{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Control.Monad.Trans
import System.Environment

import HAppS.Server
import LastFm
import Pages

main :: IO ()
main = do
    (key : secret : []) <- getArgs 
    simpleHTTP (nullConf { port = 8016 })
        [ dir "key"       [ anyRequest $ respond ok key ]
        , dir "handshake" [ withData (\session -> [anyRequest $ shakeHands key secret session]) ]
        , dir "proxy"     [ proxyServe ["*.audioscrobbler.com:80"] ]
        , withData (\token -> [ anyRequest $ createSession key secret token ]) -- Callback URL
        , root $ respond ok frontpage
        , fileServe [] "static"
        ]

-- Parsing submitted data

instance FromData Token where
    fromData = look "token"

instance FromData Session where
    fromData = do u <- look "user"
                  k <- look "key"
                  return $ Session u k

-- Handlers

createSession :: Key -> Secret -> Token -> Web Response
createSession key secret token = do
    session <- liftIO $ getSession key secret token
    case session of
        Left err                  -> respond badRequest $ layout (show err)
        Right (Session user skey) -> do
            addCookie 100000000 $ mkCookie "session" (user ++ ":" ++ skey)
            redir "."

shakeHands :: Key -> Secret -> Session -> Web Response
shakeHands key secret session = do
    handshake <- liftIO $ getHandshake key secret session
    case handshake of
        Left err                         -> respond ok $ "{error:" ++ show (show err) ++ "}"
        Right (Handshake key npurl surl) -> respond ok $ "{handshake:" ++ show [key, npurl, surl] ++ "}"

-- Utilities

root :: Monad m => WebT m a -> ServerPartT m a
root handle = withRequest isRoot
    where isRoot rq | null $ rqPaths rq = handle
                    | otherwise         = noHandle

redir :: String -> Web Response 
redir u = found u (toResponse "")

respond :: ToMessage a => (Response -> Web Response) -> a -> Web Response
respond with = with . toResponse
