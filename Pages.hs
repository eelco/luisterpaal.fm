module Pages where

import Text.XHtml.Strict

layout :: HTML a => a -> Html
layout content = header << [ thetitle << "Luisterpaal.fm :: 3VOOR12 Luisterpaal + Last.fm"
                           , javascript "http://ajax.googleapis.com/ajax/libs/jquery/1.2.6/jquery.min.js"
                           , javascript "http://ajax.googleapis.com/ajax/libs/swfobject/2.1/swfobject.js"
                           , javascript "luisterpaal.js"
                           ]
             +++ body   << content

javascript :: String -> Html
javascript uri = script ! [ thetype "text/javascript", src uri ] << noHtml

frontpage = layout $
    thediv ! [ identifier "luisterpaal" ] 
        << "Loading... (if this message doesn't go away, you might have JavaScript disabled)"
