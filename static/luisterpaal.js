var lp;    // Luisterpaal object
var sk;    // Session key
var np;    // 'Now Playing' url
var su;    // Submission url
var track; // Currently playing track

// TODO Add a div with status messages.

$(function() {
    var token = getSessionToken();

    if (token == undefined) {
        // TODO Explain to user how this will work before redirecting.
        $.get("key", {}, getNewToken)
        return;
    } // else

    // TODO Get a new token when the handshake fails
    $.getJSON("handshake/?" + $.param(token), onHandshake)
});

function onHandshake(handshake) {
    if (handshake.error != undefined) {
        alert("Ah bummer, something went wrong. (" + handshake.error + ")");
        return;
    } // else

    [sk,np,su] = handshake.handshake;
    np = np.replace('http://', '');
    su = su.replace('http://', '');


    swfobject.embedSWF("http://download.omroep.nl/vpro/luisterpaal/"+
                       "widgets/LuisterpaalEvents.swf?oninit=onLPLoad",
                       "luisterpaal","240","320","8", false, 
                       {}, {allowscriptaccess: "always"}, {});
}

function getSessionToken() {
    var token = readCookie("session");
    if (token) {
        var [ user, key ] = token.replace(/"/g,'').split(':');
        return { user: user, key: key }
    }
}

function getCallbackToken() {
    var match = window.location.search.match(/\?token=([^&]+)/)
    if (match) return match[1]
}

function getNewToken(key) {
    window.location.href = 'http://www.last.fm/api/auth?api_key=' + key;
}

function onLPLoad() {
   lp = (typeof window["luisterpaal"] != "undefined")
      ? window["luisterpaal"] 
      : document["luisterpaal"];

   // Listen to all events
   lp.addLPEventListener("START",  "start");
   lp.addLPEventListener("STOP",   "stop");
   lp.addLPEventListener("PAUSE",  "pause");
   lp.addLPEventListener("RESUME", "resume");
}

function start(e) {
    // A start is an implicit stop of the previous track
    stop();

    var t = e.track;

    track = { a: unescape(t.artist)
            , t: unescape(t.name)
            , b: unescape(t.album)
            , l: t.duration
            , n: t.trackNumber
            , i: now()
            , paused: 0
            , paused_since: undefined
            }

    // TODO Check response
    $.post("proxy/" + np
          , { s: sk
            , a: track.a
            , t: track.t
            , b: track.b
            , l: track.l
            , n: track.n
            , m: ""
            }
          );
}

function stop() {
    if (! track) return;
    // else
    
    // Don't scrobble tracks that are too short
    if (track.l < 30) return;
    // else

    // Check if the track was paused
    resume();

    var played_for = now() - track.i - track.paused;
    // Played too short
    if (played_for < 240 && played_for < track.l / 2) return;
    // else

    // TODO Check response
    $.post("proxy/" + su
          , { s: sk
            , a: track.a
            , t: track.t
            , i: track.i
            , o: "P"
            , r: ""
            , b: track.b
            , l: track.l
            , n: track.n
            , m: ""
            }
          );
}

function pause() {
    if (! track) return;
    // else
    track.paused_since = now();
}

function resume() {
    if (! track || ! track.paused_since) return;
    // else
    track.paused += now() - track.paused_since;
    track.paused_since = undefined;
}

function now() {
    return Math.round((new Date()).valueOf() / 1000);
}

// From: PPK, http://www.quirksmode.org/js/cookies.html#doccookie
function readCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
            var c = ca[i];
            while (c.charAt(0)==' ') c = c.substring(1,c.length);
            if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}
