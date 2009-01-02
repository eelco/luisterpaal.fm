module Cookies (parseCookies) where

import HAppS.Server.Cookie (Cookie(..))

import Data.Char (chr)
import Data.List ((\\))

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Hide Parsec's definitions of some Applicative functions.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- Every Monad is an Applicative.
instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

-- Every MonadPlus is an Alternative.
instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

-- Less complete but more robust way of parsing cookies.  Note: not RFC 2068 compliant!
parseCookies :: String -> [Cookie]
parseCookies str = either (const []) id $ parse cookiesParser "" str

cookiesParser = av_pairs
    where -- Parsers based on RFC 2109
          av_pairs      = (:) <$> av_pair <*> many (char ';' *> av_pair)
          av_pair       = cookie <$> attr <*> option "" (char '=' *> value)
          attr          = spaces *> token 
          value         = word
          word          = incomp_token <|> quoted_string 

          -- Parsers based on RFC 2068
          token         = many1 $ oneOf ((chars \\ ctl) \\ tspecials)
          quoted_string = char '"' *> many (oneOf qdtext) <* char '"'

          -- Custom parser, incompatible with RFC 2068, but very forgiving ;)
          incomp_token  = many1 $ oneOf ((chars \\ ctl) \\ "\";")

          -- Primitives from RFC 2068
          tspecials     = "()<>@,;:\\\"/[]?={} \t"
          ctl           = map chr (127:[0..31])
          chars         = map chr [0..127]
          octet         = map chr [0..255]
          text          = octet \\ ctl
          qdtext        = text \\ "\""

cookie key value = Cookie "" "" "" key value
