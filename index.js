// index.js
'use strict';

// include main style
require('./index.css');

const deepClone = require('./deep-clone.js');
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

//const registerToolkit = require('./toolkit.js');
// const startPatching = require('./patch.js');
const startGui = require('./gui.js');

//const LayersNode = require('./src/LayersNode.elm').LayersNode;

const buildFSS = require('./fss.js');

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const fssScenes = {};

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
            kind: layer.kind,
            config: layer.config
        };
        //scenes[index] = layer.scene;
    });
    app.ports.pause.send(null);
    app.ports.initLayers.send(layers.map((l) => l.kind));
    app.ports.import_.send(JSON.stringify({
        theta: parsedState.theta,
        size: parsedState.size,
        origin: parsedState.origin,
        mouse: parsedState.mouse,
        now: parsedState.now,
        layers: parsedState.layers.map((layer) => (
            { kind: layer.kind,
              blend: layer.blend,
              config: ''
            }
        ))
    }));
    parsedState.layers.forEach((layer, index) => {
        if (isFss(layer)) {
            const scene = buildFSS(layer.config, layer.sceneFuzz);
            scenes[index] = scene;
            app.ports.configureFss.send([ layer.config, index ]);
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
        //layer.config = layers[index] && layers[index].config ? layers[index].config : {};
        if (layer.config['lights']) {
            console.log(index, 'ambient', layer.config.lights.ambient);
            console.log(index, 'diffuse', layer.config.lights.diffuse);
        } else {
            console.log(index, 'no lights');
        }
        layer.sceneFuzz = isFss(layer)
            ? exportScene(fssScenes[index]) || exportScene(buildFSS(layer.config))
            : null;
    })
    console.log(stateObj);
    return JSON.stringify(stateObj, null, 2);
}

const exportZip_ = (app, exportedState) => {
    JSZipUtils.getBinaryContent(
        './player.bundle.js', (err, playerBundle) => {
        if (err) { throw err; }

        JSZipUtils.getBinaryContent(
            './index.player.html',
            (err, playerHtml) => {
            if (err) { throw err; }

            const sceneJson = export_(app, exportedState);
            const zip = new JSZip();
            // const js = zip.folder("js");
            // js.file('run-scene.js', runScene, { binary: true });
            // js.file(zipMinified ? 'Main.min.js' : 'Main.js', elmApp, { binary: true });
            // js.file('scene.js', 'window.jsGenScene = ' + sceneJson + ';');
            zip.file('player.bundle.js', playerBundle, { binary: true });
            zip.file('scene.js', 'window.jsGenScene = ' + sceneJson + ';');
            zip.file('index.html', playerHtml, { binary: true });
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
                import_(app, document.getElementById('import-code').value);
            } else {
                alert('Nothing to import');
            }
        } catch(e) {
            console.error(e);
            alert('Failed to parse or send, incorrect format?');
        }
    });

}

prepareImportExport();

setTimeout(function() { // FIXME: change to document.ready

    const hiddenLink = document.createElement('a');
    hiddenLink.download = 'jetbrains-art-v2.png';

    app.ports.startGui.subscribe((model) => {
        console.log('startGui', model);
        const gui = startGui(
            model.layers,
            model,
            { changeLightSpeed : index => value =>
                { app.ports.changeLightSpeed.send(Math.round(value)) }
            , changeVignette : index => value =>
                { app.ports.changeVignette.send(Math.round(value)) }
            , changeFacesX : index => value =>
                { app.ports.changeFacesX.send(Math.round(value)) }
            , changeFacesY : index => value =>
                { app.ports.changeFacesY.send(Math.round(value)) }
            , changeWGLBlend : (index, blend) =>
                { app.ports.changeWGLBlend.send({ layer: index, blend: blend }) }
            , changeSVGBlend : (index, blend) =>
                { app.ports.changeSVGBlend.send({ layer: index, blend: blend }) }
            , changeProduct : (id) =>
                { app.ports.changeProduct.send(id) }
            , setCustomSize : (value) => {
                const size = value.split(',');
                const width = parseInt(size[0]);
                const height = parseInt(size[1]);
                if (width > 0 && height > 0) {
                    app.ports.setCustomSize.send([ width, height ]);
                } else {
                    app.ports.setCustomSize.send([ window.innerWidth, window.innerHeight ]);
                }
            }
            , savePng : () => {
                var canvas = document.querySelector('.webgl-layers');
                if (!canvas) return;
                requestAnimationFrame(() => { // without that, image buffer will be empty
                    var blob = canvas.toBlob(function(blob) {
                        var url = URL.createObjectURL(blob);
                        hiddenLink.href = url;
                        hiddenLink.click();
                        //URL.revokeObjectURL(url);
                    });
                });
            }
            , changeAmplitude : index => (x, y, z) => {
                app.ports.changeAmplitude.send([ x, y, z ]);
            }
            , turnOn : index =>
                { app.ports.turnOn.send(index); }
            , turnOff : index =>
                { app.ports.turnOff.send(index); }
            });

        model.layers.forEach((layer, index) => {
            if (isFss(layer)) {
                console.log('rebuild FSS layer', index);
                const fssScene = buildFSS(model);
                app.ports.rebuildFss.send([ fssScene, index ]);
            }
        });

        app.ports.requestFssRebuild.subscribe((model) => {
            model.layers.map((layer, index) => {
                if (isFss(layer)) {
                    console.log('forced to rebuild FSS layer', index);
                    const fssScene = buildFSS(model);
                    fssScenes[index] = fssScene;
                    app.ports.rebuildFss.send([ fssScene, index ]);
                }
            });
        });
    });

    app.ports.bang.send(null);

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

}, 100);


