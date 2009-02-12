{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Control.Monad.Trans
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import HAppS.Server
import LastFm
import Pages
import Cookies

main :: IO ()
main = do
    conf <- getClientConf
    simpleHTTP (nullConf { port = 8016 }) $ serve
        [ withData    (\token -> [ anyRequest $ createSession conf token ]) -- Callback URL
        , withData    (\session -> [anyRequest $ shakeHands conf session]) 

        , root [ withDataFn userCookie (\user -> [ anyRequest $ respond ok $ welcomeBack user ])
               , anyRequest $ respond ok (welcome $ api_key conf)
               ]

        , dir "proxy" [ proxyServe ["*.audioscrobbler.com:80"] ]
        , fileServe [] "static"
        ]

-- Reading the Last.fm client config
getClientConf :: IO ClientConf
getClientConf = do
    args                  <- getArgs
    (key : secret : rest) <- readFile (head (args ++ [".last.fm"])) >>= return . words
    let (id, version) = case rest of
         []      -> ("tst", "1.0")
         [i]     -> (i    , "1.0")
         (i:v:_) -> (i    , v    )
    return $ ClientConf key secret id version

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

createSession :: ClientConf -> Token -> Web Response
createSession conf token = do
    session <- liftIO $ getSession conf token
    case session of
        Left err                  -> respond badRequest $ tryAgain (show err)
        Right (Session user skey) -> do
            addCookie 100000000 $ mkCookie "session" (user ++ ":" ++ skey)
            redir "."

shakeHands :: ClientConf -> Session -> Web Response
shakeHands conf session = do
    handshake <- liftIO $ getHandshake conf session
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

serve :: [ServerPart a] -> [ServerPart a]
serve handle = [ ServerPartT $ \rq -> unServerPartT (multi handle) (fixCookies rq) ]

fixCookies :: Request -> Request
fixCookies req = 
  if rqCookies req == [] 
      then case lookHeader "cookie" $ rqHeaders req of
              Nothing      -> req
              Just cookies -> req  { rqCookies = cookiefy $ parseCookies cookies }
      else req
  where cookiefy cs = zip (map cookieName cs) cs

lookHeader :: String -> Headers -> Maybe String
lookHeader name headers = M.lookup (B.pack name) headers >>= return . B.unpack . head . hValue
