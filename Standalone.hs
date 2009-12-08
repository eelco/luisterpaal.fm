import Luisterpaal
import Happstack.Server

main :: IO ()
main = simpleHTTP (nullConf { port = 8016 }) =<< server
