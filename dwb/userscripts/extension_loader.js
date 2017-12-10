//!javascript
//<contenthandler___SCRIPT
extensions.load("contenthandler", {
//<contenthandler___CONFIG
  // The handler can either be a string or a function, if it is a string
  // %u will be replaced with the uri of the request, if the handler is a
  // function the first parameter of the function will be the uri and the
  // function must return the command to execute.
  
  // Handle requests based on filename extension
  extension : {
    "torrent" : "transmission-remote -a '%u'",
    //"pdf" : "zathura '%u'" // Prefer to save
  },

  // Handle requests based on URI scheme
  uriScheme : {
    "magnet" : "transmission-remote -a '%u'",
    "mailto" : "dwb 'https://www.fastmail.fm/action/compose/?mailto=%u'"
  },

  // Handle requests based on MIME type
  mimeType : {
    //"application/pdf" : "zathura '%u'"
  }
//>contenthandler___CONFIG
});
//>contenthandler___SCRIPT
//<formfiller___SCRIPT
extensions.load("formfiller", {
//<formfiller___CONFIG
// shortcut that gets and saves formdata
scGetForm : "efg",

// shortcut that fills a form
scFillForm : "eff",

// path to the formdata file
formData : data.configDir + "/forms",

// whether to use a gpg-encrypted file
useGPG : true,

// your GPG key ID (leave empty to use a symmetric cipher)
GPGKeyID : "",

// whether to use a GPG agent (requires non-empty GPGKeyID to work)
GPGAgent : false,

// additional arguments passed to gpg2 when encrypting the formdata
GPGOptEncrypt : "",

// additional arguments passed to gpg2 when decrypting the formdata
GPGOptDecrypt : "",

// whether to save the password in memory when gpg is used
keepPassword : true,

// whether to save the whole formdata in memory when gpg is used
keepFormdata : false

//>formfiller___CONFIG
});
//>formfiller___SCRIPT
//<perdomainsettings___SCRIPT
extensions.load("perdomainsettings", {
//<perdomainsettings___CONFIG
// Only webkit builtin settings can be set, for a list of settings see 
// http://webkitgtk.org/reference/webkitgtk/unstable/WebKitWebSettings.html
// All settings can also be used in camelcase, otherwise they must be quoted.
// 
// The special domain suffix .tld matches all top level domains, e.g. 
// example.tld matches example.com, example.co.uk, example.it ... 
//
// Settings based on uri will override host based settings and host based
// settings will override domain based settings. Settings for domains/hosts/uris
// with without tld suffix will override settings for
// domains/hosts/uris with tld suffix respectively, e.g. 
//      "example.com" : { enableScripts : true }, 
//      "example.tld" : { enableScripts : false } 
// will enable scripts on example.com but not on example.co.uk, example.it, ... 


// Settings applied based on the second level domain
domains : {
//      "example.com" : { "auto-load-images" : false }, 
//      "google.tld" : { enableScripts : false, autoLoadImages : false }, 
},

//Settings applied based on the hostname
hosts : {
//    "www.example.com" : { autoLoadImages : true } 
},

// Settings applied based on the uri
uris : {
//  "http://www.example.com/foo/" : { autoLoadImages : true } }, 
},

//>perdomainsettings___CONFIG
});
//>perdomainsettings___SCRIPT
//<requestpolicy___SCRIPT
extensions.load("requestpolicy", {
//<requestpolicy___CONFIG
// path to a whitelist 
whiteList : data.configDir + "/" + data.profile + "/requestpolicy.json",

// shortcut to block/allow requests
shortcut : "erp",

// shortcut to unblock requests from current site that are blocked on all
// sites
unblockCurrent : "erC",

// shortcut to unblock requests that are blocked on all sites
unblockAll : "erA",

// reload current site after blocking / unblocking a request
autoreload : false,

// notify about blocked requests
notify : true

//>requestpolicy___CONFIG
});
//>requestpolicy___SCRIPT
//<userscripts___SCRIPT
extensions.load("userscripts", {
//<userscripts___CONFIG
  // paths to userscripts, this extension will also load all scripts in 
  // $XDG_CONFIG_HOME/dwb/greasemonkey, it will also load all scripts in
  // $XDG_CONFIG_HOME/dwb/scripts but this is deprecated and will be
  // disabled in future versions.
  scripts : []
//>userscripts___CONFIG
});
//>userscripts___SCRIPT
//<navtools___SCRIPT
extensions.load("navtools", {
//<navtools___CONFIG
// Shortcut for navigating to the next page
forwardBind : "]]",

// Shortcut for navigating to the previous page
backBind : "[[",

// Go up one directory, e.g. from http://www.example.com/foo/bar to 
// http://www.example.com/foo
updirBind : "gu", 

// Go to top directory, e.g. from http://www.example.com/foo/bar to 
// http://www.example.com
topdirBind : "gU", 

// Patterns to match against when searching for "next"-links
nextPatterns : "next,more,>,\u2192,\xbb,\u226b,>>",

// Patterns to match against when searching for "previous"-links
previousPatterns : "prev,previous,back,<,\u2190,\xab,\u226a,<<"

//>navtools___CONFIG
});
//>navtools___SCRIPT
