var lp;    // Luisterpaal object
var sk;    // Session key
var np;    // 'Now Playing' url
var su;    // Submission url
var track; // Currently playing track
var np_to; // 'Now Playing' timeout

// TODO Add a div with status messages.

$(function() {
    $('button[name=yes]').click(function(){
        $('#content').fadeOut("normal", function(){ $(this).hide() });
        $('#luisterpaal').show();
    });

    $('button[name=no]').click(function(){
        eraseCookie("session");
        window.location.href = '.'; // Reload
    });
    
    // Initialize luisterpaal
    var token = getSessionToken();

    if (token == undefined) return;
    // else

    // TODO Get a new token when the handshake fails
    $.getJSON("?" + $.param(token), onHandshake)
});

function onHandshake(response) {
    if (response.error != undefined) {
        alert("Ah verdorie, er ging iets mis. (" + response.error + ")");
        return;
    } // else

    var handshake = response.handshake;
    sk = handshake[0];
    np = handshake[1].replace('http://', '');
    su = handshake[2].replace('http://', '');

    swfobject.embedSWF("http://download.omroep.nl/vpro/luisterpaal/"+
                       "widgets/LuisterpaalEvents.swf?oninit=onLPLoad",
                       "luisterpaal","240","320","8", false, 
                       {}, {allowscriptaccess: "always"}, {});
}

function getSessionToken() {
    var token = readCookie("session");
    if (token) {
        var session = token.replace(/"/g,'').split(':');
        return { user: session[0], key: session[1] }
    }
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

    unvariousArtist()

    // Clear the call, mainly to prevent submitting data too soon (because if
    // 'track' updated in the mean time that's okay).
    clearTimeout(np_to);

    np_to = setTimeout(function() {
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
    }, 10000); // Set now playing info after 10 seconds.
}

function stop() {
    if (! track) return;
    // else
    
    // If now playing info was not yet send, don't do it
    clearTimeout(np_to);

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
          , { "s"   : sk
            , "a[0]": track.a
            , "t[0]": track.t
            , "i[0]": track.i
            , "o[0]": "P"
            , "r[0]": ""
            , "b[0]": track.b
            , "l[0]": track.l
            , "n[0]": track.n
            , "m[0]": ""
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
    return Math.round(new Date().getTime() / 1000);
}

function unvariousArtist() {
    if (track.a.match(/diverse artiesten|various artists/i) != null) {
        track.a = track.t.split(' - ')[0];
        track.t = track.t.split(' - ').slice(1).join(' - ');
    }
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

function eraseCookie(name) {
    var date = new Date();
	date.setTime(date.getTime() - 3600);
	document.cookie = name+"="+";expires="+date.toGMTString()+"; path=/";
}
