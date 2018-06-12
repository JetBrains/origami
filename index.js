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

registerToolkit(app, BlendsNode);

// const startLorenz = require('./lorenz.js');

const addFSS = require('./fss.js');

// startLorenz(app);

addFSS(app.ports.rebuildFss, app.ports.configureFss, 0);
//app.ports.configureFss.send({ colors : [ '#4b4e76', '#fb4e76' ]});
//addFSS(app.ports.rebuildFss, app.ports.configureFss, 1);
startPatching();

