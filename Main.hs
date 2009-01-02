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
        [ withData    (\token -> [ anyRequest $ createSession key secret token ]) -- Callback URL
        , withData    (\session -> [anyRequest $ shakeHands key secret session]) 

        , root [ withDataFn userCookie (\user -> [ anyRequest $ respond ok $ welcomeBack user ])
               , anyRequest $ respond ok (welcome key)
               ]

        , dir "proxy" [ proxyServe ["*.audioscrobbler.com:80"] ]
        , fileServe [] "static"
        ]

-- Parsing submitted data

instance FromData Token where
    fromData = look "token"

instance FromData Session where
    fromData = do u <- look "user"
                  k <- look "key"
                  return $ Session u k

userCookie :: RqData User
userCookie = lookCookieValue "session" >>= return . takeWhile (/=':')

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

root :: Monad m => [ServerPartT m a] -> ServerPartT m a
root handle = ServerPartT $ \rq -> case rqPaths rq of
                [] -> unServerPartT (multi handle) rq
                _  -> noHandle

redir :: String -> Web Response 
redir u = found u (toResponse "")

respond :: ToMessage a => (Response -> Web Response) -> a -> Web Response
respond with = with . toResponse
