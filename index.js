// index.js
'use strict';

// include main style
require('./index.css');

const JSZip = require('jszip');
const JSZipUtils = require('jszip-utils');
const FileSaver = require('jszip/vendor/FileSaver');

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

function deepClone(obj) {
    return JSON.parse(JSON.stringify(obj))
}

const defaultConfig =
    { lights:
        { ambient: [ '#000000', '#f45b69' ]
        , diffuse:  [ '#000000', '#e4fde1' ]
        , count: 2
        }
    , material: [ '#ffffff', '#ffffff' ]
    , xRange: 0.8
    , yRange: 0.1
    , size: [ 3550, 3200 ]
    , faces: [ 35, 35 ]
    , mirror: 0.5
    };

let layerOneConfig = deepClone(defaultConfig);
layerOneConfig.lights.ambient = [ '#000000', '#f45b69' ];
layerOneConfig.lights.diffuse = [ '#000000', '#e4fde1' ];

let layerTwoConfig = deepClone(defaultConfig);
layerTwoConfig.lights.ambient = [ '#000000', '#4b4e76' ];
layerOneConfig.lights.diffuse = [ '#000000', '#fb4e76' ];

let layers = [
    { type: 'fss-mirror', config: layerOneConfig
        /* { ...defaultConfig
        , lights:
            { ambient: [ '#000000', '#f45b69' ]
            , diffuse:  [ '#000000', '#e4fde1' ]
            , count: 2
            }
        } */
    },
    { type: 'fss-mirror', config: layerTwoConfig
        /* { ...defaultConfig
        , lights:
            { ambient: [ '#000000', '#4b4e76' ]
            , diffuse:  [ '#000000', '#fb4e76' ]
            , count: 2
            }
        } */
    },
    { type: 'text' }
];

let scenes = {};

let layersNode = null;
let paletteNode = null;

const updateFssLayer = (index, config) => {
    const scene = buildFSS(config);
    app.ports.configureMirroredFss.send([ config, index ]);
    app.ports.rebuildFss.send([ scene, index ]);
    if (layers[index]) {
        layers[index].config = deepClone(config);
    }
    scenes[index] = scene;
}

const updateAllFssLayers = (updateConfig) => {
    layers.forEach((layer, index) => {
        if (layer.type == 'fss-mirror') {
            updateFssLayer(index,
                updateConfig(deepClone(layer.config)));
        }
    });
}

const updateFssColors = (index, colors) => {
    let newConfig = deepClone(defaultConfig);
    newConfig.lights.ambient[1] = colors[0];
    newConfig.lights.diffuse[1] = colors[1];
    updateFssLayer(index, newConfig);
}

const exportScene = (scene) => {
    //console.log(scene);
    return scene.meshes[0].geometry.vertices.map((vertex) => (
        { v0: vertex.v0,
          time: vertex.time,
          anchor: vertex.anchor,
          gradient: vertex.gradient
        }
    ));
}

const import_ = (app, importedState) => {
    const parsedState = JSON.parse(importedState);
    scenes = {};
    layers = [];
    parsedState.layers.forEach((layer, index) => {
        layers[index] = {
            type: layer.type,
            config: layer.config
        };
        //scenes[index] = layer.scene;
    });
    app.ports.pause.send(null);
    app.ports.initLayers.send(layers.map((l) => l.type));
    app.ports.import_.send(JSON.stringify({
        theta: parsedState.theta,
        size: parsedState.size,
        mouse: parsedState.mouse,
        now: parsedState.now,
        layers: parsedState.layers.map((layer) => (
            { type_ : layer.type,
                blend: layer.blend,
                config: ''
            }
        ))
    }));
    parsedState.layers.forEach((layer, index) => {
        if (layer.type == 'fss-mirror') {
            const scene = buildFSS(layer.config, layer.sceneFuzz);
            scenes[index] = scene;
            app.ports.configureMirroredFss.send([ layer.config, index ]);
            app.ports.rebuildFss.send([ scene, index ]);
        }
    });
    const mergedBlends = parsedState.layers.map(layer => layer.blend).join(':');
    window.location.hash = '#blends=' + mergedBlends;
    //if (layersNode) layersNode.inlets['code'].receive(mergedBlends);
}

const export_ = (app, exportedState) => {
    app.ports.pause.send(null);
    const stateObj = JSON.parse(exportedState);
    stateObj.layers.forEach((layer, index) => {
        layer.config = layers[index] ? layers[index].config : {};
        console.log(index, 'ambient', layer.config.lights.ambient);
        console.log(index, 'diffuse', layer.config.lights.diffuse);
        layer.sceneFuzz = layer.type == 'fss-mirror'
            ? exportScene(scenes[index]) || exportScene(buildFSS(layer.config))
            : null;
    })
    console.log(stateObj);
    return JSON.stringify(stateObj, null, 2);
}

const zipMinified = false;
const exportZip_ = (app, exportedState) => {
    JSZipUtils.getBinaryContent('./run-scene.js', (err, runScene) => {
        if (err) { throw err; }

        JSZipUtils.getBinaryContent(
            zipMinified ? './build/Main.min.js' : './build/Main.js',
            (err, elmApp) => {
            if (err) { throw err; }

            const sceneJson = export_(app, exportedState);
            const zip = new JSZip();
            const js = zip.folder("js");
            js.file('run-scene.js', runScene, { binary: true });
            js.file(zipMinified ? 'Main.min.js' : 'Main.js', elmApp, { binary: true });
            js.file('scene.js', 'window.jsGenScene = ' + sceneJson + ';');
            zip.file('index.html', '<!doctype html><html>'
                + '<head>'
                    + '<meta charset="utf-8" />'
                    + (zipMinified
                            ? '<script src="./js/Main.min.js"></script>'
                            : '<script src="./js/Main.js"></script>')
                    + '<script src="./js/scene.js"></script>'
                    + '<script src="./js/run-scene.js"></script>'
                + '</head>'
                + '<body>'
                + '<div id="app"></div>'
                    + '<script>if (window.runGenScene) { window.runGenScene(); } else { console.error(\'Scene not found\') }</script>'
                + '</body>'
            + '</html>');
            zip.generateAsync({type:"blob"})
                .then(function(content) {
                    new FileSaver(content, "export.zip");
                });
        });
    });
}

const prepareImportExport = () => {
    app.ports.export_.subscribe((exportedState) => {
        const exportCode = export_(app, exportedState);

        document.getElementById('export-target').className = 'shown';
        document.getElementById('export-code').value = exportCode;
    });
    app.ports.exportZip_.subscribe((exportedState) => {
        try {
            console.log('exportedState', exportedState);
            exportZip_(app, exportedState);
        } catch(e) {
            console.error(e);
            alert('Failed to create .zip');
        }
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
                const importCode = JSON.parse(document.getElementById('import-code').value);
                import_(app, importCode);
            } else {
                alert('Nothing to import');
            }
        } catch(e) {
            console.error(e);
            alert('Failed to parse or send, incorrect format?');
        }
    });

}

const resize = () => {
    layers = layers.map((layer) => {
        if (layer.type == 'fss-mirror') {
            const newLayer = deepClone(layer);
            newLayer.config.size = [Math.floor(window.innerWidth * 2.5), Math.floor(window.innerHeight * 2.5)];
            //newLayer.config.faces = [Math.floor(window.innerWidth / 300), Math.floor(window.innerHeight / 300)];
            return newLayer;
        } else {
            return layer;
        }
    });
    console.log(layers.map(layer => {
        return (layer.type == 'fss-mirror') ? [layer.config.size, layer.config.faces] : [];
    }));
}

const rebuild = () => {
    layers.forEach((layer, index) => {
        if (layer.type == 'fss-mirror') {
            updateFssLayer(index, layer.config);
        }
    });
}

registerToolkit(app, LayersNode, updateFssColors);

app.ports.initLayers.send(layers.map((l) => l.type));

prepareImportExport();

setTimeout(function() {
    resize();
    rebuild();

    const nodes = startPatching(layers, updateAllFssLayers);

    layersNode = nodes.layersNode;
    paletteNode = nodes.paletteNode;

    let panelsHidden = false;

    document.addEventListener('keydown', (event) => {
        if (event.keyCode == 32) {
            const overlayPanels = document.querySelectorAll('.hide-on-space');
            for (let i = 0; i < overlayPanels.length; i++) {
                overlayPanels[i].style.display = panelsHidden ? 'block' : 'none';
            }
            panelsHidden = !panelsHidden;
        }
      });

    window.addEventListener('resize', () => {
        resize();
        rebuild();
    });

    // setTimeout(function() {
    //     //updateFssColors(0, ['#000000', '#ffffff']);
    //     updateFssColors(1, ['#ffffff', '#000000']);
    // }, 100);
}, 100);


