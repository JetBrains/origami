// index.js
'use strict';

// include main style
require('./index.css');

// initialize Elm Application
var Lorenz = require('./src/Main.elm');
var mountNode = document.getElementById('elm-target');

// Prepare JB-Toolkit
var registerToolkit = require('./toolkit.js');

// The third value on embed are the initial values for incomming ports into Elm
var app = Lorenz.Main.embed(mountNode);

var BlendsNode = require('./src/BlendsNode.elm').BlendsNode;

registerToolkit(app, BlendsNode);

// Prepare RPD-patch
var startPatch = require('./patch.js');

require('./fss.js');

startPatch(app);
