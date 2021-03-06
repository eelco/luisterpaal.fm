module Pages where

import Text.XHtml.Strict

layout :: HTML a => a -> Html
layout content = header << [ thetitle << "Luisterpaal.fm :: 3VOOR12 Luisterpaal + Last.fm"
                           , javascript "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"
                           , javascript "http://ajax.googleapis.com/ajax/libs/swfobject/2.2/swfobject.js"
                           , javascript "http://luisterpaal.uservoice.com/pages/general/widgets/tab.js?alignment=right&color=00BCBA"
                           , javascript "luisterpaal.js"
                           , stylesheet "style.css"
                           ]
             +++ body   << content

javascript :: String -> Html
javascript uri = script ! [ thetype "text/javascript", src uri ] << noHtml

stylesheet :: String -> Html
stylesheet uri = thelink noHtml ! [ rel "stylesheet", href uri ]

luisterpaal :: HTML a => a -> Html
luisterpaal content = layout 
    [ thediv ! [ identifier "luisterpaal" ] << ""
    , thediv ! [ identifier "content"     ] << content
    ]

welcome key = layout 
    [ h1 << "Welkom!"
    , h2 << "Scrobble al je geluisterpaal naar Last.fm!"
    , p  << "Lekker naar de 3VOOR12 Luisterpaal luisteren terwijl ook je Last.fm profiel gespekt wordt, nu kan het!"
    , h2 << "Zo werkt het"
    , p  << ("Je hoeft " +++ strong << "niet" +++ " je Last.fm wachtwoord te geven, dat zou niet veilig zijn, dus daar hebben ze bij Last.fm iets op bedacht.")
    , p  << "Zodra je op de knop drukt, word je even omgeleid via Last.fm, waar je toestemming geeft aan de Luisterpaal Scrobbler om te scrobbelen."
    , p  << "Daarna wordt je weer hier naartoe gestuurd, en zal de 3VOOR12 Luisterpaal verschijnen."
    , p  << "Om te voorkomen dat je deze handeling steeds moet uitvoeren, krijg je een cookie met daarin een code die uniek is voor de afspraak die jij, Last.fm en de Luisterpaal Scrobbler hebben gemaakt."
    , form ! [ action $ "http://www.last.fm/api/auth" ] << 
        [ hidden "api_key" key 
        , input ! [ thetype "submit", value "Klinkt goed. Let's go!" ]
        ]
    ]

welcomeBack user = luisterpaal
    [ h1 << ("Welkom terug, " ++ user ++ "!")
    , p  << "Goed je weer te zien.  Klaar om de 3VOOR12 Luisterpaal te starten?"
    , button ! [ name "yes" ] << "Ja, starten maar!"
    , button ! [ name "no" ]  << ("Nee, wacht!  Ik ben niet " ++ user)
    ]

tryAgain err = layout
    [ p << strong << "Er ging iets mis in de communicatie met Last.fm"
    , p << err
    , p << hotlink "#" ! [ strAttr "onclick" "javascript:location.reload();" ] << "Probeer opnieuw"
    ]
