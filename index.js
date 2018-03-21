// index.js
'use strict';

// include main style
require('./index.css');

// initialize Elm Application
const Lorenz = require('./src/Main.elm');
const mountNode = document.getElementById('elm-target');

// Prepare JB-Toolkit
const registerToolkit = require('./toolkit.js');

// The third value on embed are the initial values for incomming ports into Elm
const app = Lorenz.Main.embed(mountNode);

const BlendsNode = require('./src/BlendsNode.elm').BlendsNode;

//registerToolkit(app, BlendsNode);

// Prepare RPD-patch
//const startPatch = require('./patch.js');

const startFss = require('./fss.js');

//startPatch(app);
startFss(app.ports.receiveFss/*, app.ports.changeFss*/);
