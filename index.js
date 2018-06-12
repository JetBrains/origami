// index.js
'use strict';

// include main style
require('./index.css');

// initialize Elm Application
const App = require('./src/Main.elm');
//const mountNode = document.getElementById('elm-target');
const mountNode = document.getElementById('js-animation');
// The third value on embed are the initial values for incomming ports into Elm
const app = App.Main.embed(mountNode);

// mountNode.addEventListener('click', function() {
//     app.ports.pause.send(null);
// });

const registerToolkit = require('./toolkit.js');
const startPatching = require('./patch.js');

const BlendsNode = require('./src/BlendsNode.elm').BlendsNode;

// Prepare JB-Toolkit
registerToolkit(app, BlendsNode);

// Prepare RPD-patch
// const startLorenz = require('./lorenz.js');

const startFss = require('./fss.js');

// startLorenz(app);

startFss(app.ports.rebuildFss, app.ports.configureFss);
startPatching();

