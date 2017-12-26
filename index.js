// index.js
'use strict';

require('./node_modules/rpd/src/render/svg.css');
require('./node_modules/rpd/src/render/shared.js');
require('./node_modules/rpd/src/render/svg.js');

require('./node_modules/rpd/src/style/ableton/svg.css');
require('./node_modules/rpd/src/style/ableton/svg.js');

var Rpd = require('./node_modules/rpd/src/rpd.js');
console.log(Rpd);

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elm-app');

// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);
