module Main where

import Luisterpaal
import Happstack.Server.FastCGI
import Control.Concurrent

main :: IO ()
main = runFastCGI . serverPartToCGI =<< server
