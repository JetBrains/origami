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

const size = [ 1550, 800 ];
const faces = [ 10, 10 ];
const mirrorPos = 0.5;
const colorsA = [ '#f45b69', '#e4fde1' ];
const colorsB = [ '#4b4e76', '#fb4e76' ];

const layers = [
    { type: 'fss-mirror', config:
        { colors: colorsA
        , size: size
        , faces: faces
        , mirror: mirrorPos
        }
    },
    { type: 'fss-mirror', config:
        { colors: colorsB
        , size: size
        , faces: faces
        , mirror: mirrorPos
        }
    }
];

const updateFssLayer = (index, config) => {
    const scene = buildFSS(config);
    app.ports.configureMirroredFss.send([ config, index ]);
    app.ports.rebuildFss.send([ scene, index ]);
    if (layers[index]) {
        layers[index].config = config;
    }
}

const updateFssColors = (index, colors) => {
    const newConfig =
        { colors: colors
        , size: size // 4000, 4000
        , faces: faces // 50, 80
        , mirror: mirrorPos }
    updateFssLayer(index, newConfig);
}

registerToolkit(app, LayersNode, updateFssColors);

const buildFSS = require('./fss.js');

app.ports.initLayers.send(layers.map((l) => l.type));

app.ports.export_.subscribe(function(exportedState) {
    const stateObj = JSON.parse(exportedState);
    stateObj.layers.forEach((layer, index) => {
        layer.config = layers[index] ? layers[index].config : {};
    })
    console.log(stateObj);

    document.getElementById('export-target').className = 'shown';
    document.getElementById('export-code').value = JSON.stringify(stateObj);
});
document.getElementById('close-export').addEventListener('click', () => {
    document.getElementById('export-target').className = '';
});

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


