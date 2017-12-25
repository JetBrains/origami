// index.js
'use strict';

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elm-app');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);
