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

const buildFSS = require('./fss.js');

const size = [ 1550, 800 ];
const faces = [ 10, 10 ];
const mirrorPos = 0.5;
const colorsA = [ '#f45b69', '#e4fde1' ];
const colorsB = [ '#4b4e76', '#fb4e76' ];

let layers = [
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

let scenes = {};
const updateFssLayer = (index, config) => {
    const scene = buildFSS(config);
    app.ports.configureMirroredFss.send([ config, index ]);
    app.ports.rebuildFss.send([ scene, index ]);
    if (layers[index]) {
        layers[index].config = config;
    }
    scenes[index] = scene;
}

const updateFssColors = (index, colors) => {
    const newConfig =
        { colors: colors
        , size: size // 4000, 4000
        , faces: faces // 50, 80
        , mirror: mirrorPos }
    updateFssLayer(index, newConfig);
}

const prepareImportExport = () => {
    app.ports.export_.subscribe(function(exportedState) {
        app.ports.pause.send(null);
        const stateObj = JSON.parse(exportedState);
        stateObj.layers.forEach((layer, index) => {
            layer.config = layers[index] ? layers[index].config : {};
            layer.scene = layer.type == 'fss-mirror'
                ? scenes[index] || buildFSS(layer.config)
                : null;
        })
        console.log(stateObj);

        document.getElementById('export-target').className = 'shown';
        document.getElementById('export-code').value = JSON.stringify(stateObj, null, 2);
    });
    document.getElementById('close-export').addEventListener('click', () => {
        document.getElementById('export-target').className = '';
    });
    document.getElementById('close-import').addEventListener('click', () => {
        document.getElementById('import-target').className = '';
    });
    setTimeout(() => {
        document.getElementById('import-button').addEventListener('click', () => {
            document.getElementById('import-target').className = 'shown';
        });
    }, 100);
    document.getElementById('import').addEventListener('click', () => {
        try {
            if (document.getElementById('import-code').value) {
                const parsedState = JSON.parse(document.getElementById('import-code').value);
                scenes = {};
                layers = [];
                parsedState.layers.forEach((layer, index) => {
                    layers[index] = {
                        type: layer.type,
                        config: layer.config
                    };
                    scenes[index] = layer.scene;
                });
                app.ports.pause.send(null);
                app.ports.initLayers.send(layers.map((l) => l.type));
                app.ports.import_.send(JSON.stringify({
                    theta: parsedState.theta,
                    size: parsedState.size,
                    mouse: parsedState.mouse,
                    time: parsedState.time,
                    layers: parsedState.layers.map((layer) => (
                        { type_ : layer.type,
                          blend: layer.blend
                        }
                    ))
                }));
                parsedState.layers.forEach((layer, index) => {
                    if (layer.type == 'fss-mirror') {
                        const scene = layer.scene || buildFSS(layer.config);
                        app.ports.configureMirroredFss.send([ layer.config, index ]);
                        app.ports.rebuildFss.send([ scene, index ]);
                    }
                });
            } else {
                alert('Nothing to import');
            }
        } catch(e) {
            console.error(e);
            alert('Failed to parse or send, incorrect format?');
        }
    });
}

registerToolkit(app, LayersNode, updateFssColors);

app.ports.initLayers.send(layers.map((l) => l.type));

prepareImportExport();

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


