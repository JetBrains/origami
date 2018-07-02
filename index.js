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

const LayersNode = require('./src/LayersNode.elm').LayersNode;

const layers = [
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

const updateFssLayer = (index, config) => {
    const scene = buildFSS(config);
    app.ports.configureMirroredFss.send([ config, index ]);
    app.ports.rebuildFss.send([ scene, index ]);
}

const updateFssColors = (index, colors) => {
    const newConfig =
        { colors: colors
        , size: [ 1550, 800 ] // 4000, 4000
        , faces: [ 10, 10 ] // 50, 80
        , mirror: 0.5 }
    updateFssLayer(index, newConfig);
}

registerToolkit(app, LayersNode, updateFssColors);

const buildFSS = require('./fss.js');

app.ports.initLayers.send(layers.map((l) => l.type));

setTimeout(function() {
    layers.forEach((layer, index) => {
        if (layer.type == 'fss-mirror') {
            updateFssLayer(index, layer.config);
        }
    });

    startPatching(layers);

    // setTimeout(function() {
    //     //updateFssColors(0, ['#000000', '#ffffff']);
    //     updateFssColors(1, ['#ffffff', '#000000']);
    // }, 100);
}, 100);


