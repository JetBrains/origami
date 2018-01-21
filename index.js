// index.js
'use strict';

// include main style
require('./index.css');

// Prepare JB-Toolkit
require('./toolkit.js');

// Prepare RPD-patch
var startPatch = require('./patch.js');

// initialize Elm Application
var Elm = require('./src/Main.elm');
var mountNode = document.getElementById('elm-target');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);

startPatch(app);
