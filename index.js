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

mountNode.addEventListener('click', function(){
    app.ports.pause.send(null);
});


// Prepare JB-Toolkit
const registerToolkit = require('./toolkit.js');

const BlendsNode = require('./src/BlendsNode.elm').BlendsNode;

registerToolkit(app, BlendsNode);

// Prepare RPD-patch
const startPatch = require('./patch.js');

const startFss = require('./fss.js');

startPatch(app);

startFss(app.ports.rebuildFss, app.ports.configureFss);


