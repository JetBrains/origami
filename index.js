// index.js
'use strict';

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elm-app');

var Rpd = require('./node_modules/rpd/src/rpd.js');
console.log(Rpd);

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);
