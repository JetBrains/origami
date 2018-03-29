// index.js
'use strict';

// include main style
require('./index.css');

// initialize Elm Application
const Fss = require('./src/MainFSS.elm');
const mountNode = document.getElementById('js-animation');

// The third value on embed are the initial values for incomming ports into Elm
const app = Fss.Main.embed(mountNode);

const startFss = require('./fss.js');

startFss(app.ports.receiveFss/*, app.ports.changeFss*/);


