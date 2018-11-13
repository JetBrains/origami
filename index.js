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
        omega: parsedState.omega,
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
            const scene = buildFSS(model, layer.config, layer.sceneFuzz);
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
        layer.sceneFuzz = isFss(layer)
            ? exportScene(fssScenes[index]) || exportScene(buildFSS(model, layer.model))
            : null;
    })
    console.log(stateObj);
    return JSON.stringify(stateObj, null, 2);
}

const waitForContent = filePath => {
    return new Promise((resolve, reject) => {
        JSZipUtils.getBinaryContent(filePath, (err, fileContent) => {
            if (err) { reject(err); return; }
            console.log('packing ' + filePath);
            resolve(fileContent)
        });
    });
};

const exportZip_ = (app, exportedState) => {
    // waitForContent
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
            const assets = zip.folder('assets');
            const assetPromises =
                [ 'appcode', 'clion', 'datagrip', 'dotcover', 'dotmemory', 'dotpeek'
                , 'dottrace', 'gogland', 'hub', 'intellij-idea-ce', 'intellij-idea'
                , 'jetbrains-simple', 'jetbrains', 'kotlin', 'mps', 'phpstorm'
                , 'pycharm-ce', 'pycharm-edu', 'pycharm', 'resharper', 'resharper-cpp'
                , 'resharper', 'rider', 'rubymine', 'teamcity', 'test', 'toolbox', 'upsource'
                , 'webstorm', 'youtrack' ]
                    .map(productId => './assets/' + productId + '.svg')
                    .map(waitForContent);
            Promise.all(assetPromises)
                   .then(files =>
                        files.map(content => assets.file('rr', content, { binary: true }))
                    )
                   .then(() => zip.generateAsync({type:"blob"}))
                   .then(content => new FileSaver(content, "export.zip"));
            // zip.generateAsync({type:"blob"})
            //     .then(function(content) {
            //         new FileSaver(content, "export.zip");
            //     });
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

const savePng = (hiddenLink) => {
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

prepareImportExport();

setTimeout(function() { // FIXME: change to document.ready

    const hiddenLink = document.createElement('a');
    hiddenLink.download = 'jetbrains-art-v2.png';

    // app.ports.presetSizeChanged.subscribe(size => setTimeout(() => savePng(hiddenLink), 0));

    app.ports.startGui.subscribe((model) => {
        console.log('startGui', model);
        startGui(
            document,
            model,
            { changeLightSpeed : index => value =>
                { app.ports.changeLightSpeed.send({ layer: index, value: Math.round(value) }) }
            , changeVignette : index => value =>
                { app.ports.changeVignette.send({ layer: index, value: Math.round(value) }) }
            , changeIris : index => value =>
                { app.ports.changeIris.send({ layer: index, value: Math.round(value) }) }
            , changeFacesX : index => value =>
                { app.ports.changeFacesX.send({ layer: index, value: Math.round(value) }) }
            , changeFacesY : index => value =>
                { app.ports.changeFacesY.send({ layer: index, value: Math.round(value) }) }
            , changeRenderMode : index => renderMode =>
                { app.ports.changeFssRenderMode.send({ layer: index, value: renderMode }) }
            , changeWGLBlend : (index, blend) =>
                { app.ports.changeWGLBlend.send({ layer: index, value: blend }) }
            , changeSVGBlend : (index, blend) =>
                { app.ports.changeSVGBlend.send({ layer: index, value: blend }) }
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
            , savePng : () => savePng(hiddenLink)
            , saveBatch : (sizes) => {
                sizes.forEach(([width, height]) => {
                    if (width > 0 && height > 0) {
                        app.ports.setCustomSize.send([ width, height ]);
                    }
                    // setTimeout(() => savePng(hiddenLink), 0);
                });
            }
            , changeAmplitude : index => (x, y, z) => {
                app.ports.changeAmplitude.send({ layer: index, value: [ x, y, z ]});
            }
            , turnOn : index =>
                { app.ports.turnOn.send(index); }
            , turnOff : index =>
                { app.ports.turnOff.send(index); }
            , mirrorOn : index =>
                { app.ports.mirrorOn.send(index); }
            , mirrorOff : index =>
                { app.ports.mirrorOn.send(index); }    
            , rotate : value => 
                { app.ports.rotate.send(value); }
            });

        model.layers.forEach((layer, index) => {
            if (isFss(layer)) {
                console.log('rebuild FSS layer', index);
                const fssScene = buildFSS(model, model.fss);
                app.ports.rebuildFss.send({ value: fssScene, layer: index });
            }
        });

        app.ports.requestFssRebuild.subscribe(({ layer : index, model, value : fssModel }) => {
            const layer = model.layers[index];
            if (isFss(layer)) {
                console.log('forced to rebuild FSS layer', index);
                const fssScene = buildFSS(model, fssModel);
                fssScenes[index] = fssScene;
                app.ports.rebuildFss.send({ value: fssScene, layer: index });
                layer.scene = fssScene;
            }
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


