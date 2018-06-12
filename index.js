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

const buildFSS = require('./fss.js');

// startLorenz(app);
var layers = [
    { type: 'fss-mirror', config:
        { colors: [ '#f45b69', '#e4fde1' ]
        , size: [ 1550, 800 ] // 4000, 4000
        , faces: [ 10, 10 ] // 50, 80
        , mirror: 0.5 }
    },
    { type: 'fss-mirror', config:
        { colors: [ '#4b4e76', '#fb4e76' ]
        , size: [ 1550, 800 ] // 4000, 4000
        , faces: [ 10, 10 ] // 50, 80
        , mirror: 0.5 }
    }
];

app.ports.initLayers.send(layers.map((l) => l.type));

setTimeout(function() {
    layers.forEach((layer, index) => {
        if (layer.type == 'fss-mirror') {
            const scene = buildFSS(layer.config);
            app.ports.configureMirroredFss.send([ layer.config, index ]);
            app.ports.rebuildFss.send([ scene, index ]);
        }
    });

    startPatching(layers);
}, 100);


