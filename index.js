// index.js
'use strict';

// include main style
require('./index.css');

const deepClone = require('./deep-clone.js');
const drawToCanvas = require('./draw-to-canvas.js');
const JSZip = require('jszip');
const JSZipUtils = require('jszip-utils');
const FileSaver = require('jszip/vendor/FileSaver');

// initialize Elm Application
const App = require('./src/Main.elm');
//const mountNode = document.getElementById('elm-target');
const mountNode = document.getElementById('js-animation');
// The third value on embed are the initial values for incomming ports into Elm
const app = App.Main.embed(mountNode);

const startGui = require('./gui.js');
const buildFSS = require('./fss.js');

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const fssScenes = {};

const batchPause = 1000;
let savingBatch = false;

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

const prepareModelForImport = (model) => {
    const toSend = deepClone(model);
    toSend.layers =
        model.layers.map(layerDef => {
            const layerDef_ = deepClone(layerDef);
            layerDef_.model = JSON.stringify(layerDef.model);
            return layerDef_;
        });
    //console.log('sending for the import', toSend);

    return toSend;
}

const import_ = (app, parsedState) => {
    const preparedModel = prepareModelForImport(parsedState);
    app.ports.import_.send(JSON.stringify(preparedModel));

    parsedState.layers.map((layer, index) => {
        if (isFss(layer)) {
            const fssScene = buildFSS(parsedState, layer.model, layer.sceneFuzz);
            fssScenes[index] = fssScene;
            app.ports.rebuildFss.send({ value: fssScene, layer: index });
        }
    });

    app.ports.pause.send(null);
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
    return {
        source: stateObj,
        json: JSON.stringify(stateObj, null, 2)
    };
}

const waitForContent = ({ path, id, name }) => {
    return new Promise((resolve, reject) => {
        JSZipUtils.getBinaryContent(path, (err, content) => {
            if (err) { reject(err); return; }
            // console.log('packing ' + path);
            resolve({ path, id, name, content });
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

            const { json, source }  = export_(app, exportedState);
            const zip = new JSZip();
            // const js = zip.folder("js");
            // js.file('run-scene.js', runScene, { binary: true });
            // js.file(zipMinified ? 'Main.min.js' : 'Main.js', elmApp, { binary: true });
            // js.file('scene.js', 'window.jsGenScene = ' + json + ';');
            zip.file('player.bundle.js', playerBundle, { binary: true });
            zip.file('scene.js', 'window.jsGenScene = ' + json + ';');
            zip.file('index.html', playerHtml, { binary: true });
            const assets = zip.folder('assets');
            const assetPromises =
                [ source.product ]
                // [ 'appcode', 'clion', 'datagrip', 'dotcover', 'dotmemory', 'dotpeek'
                // , 'dottrace', 'gogland', 'hub', 'intellij-idea-ce', 'intellij-idea'
                // , 'jetbrains-simple', 'jetbrains', 'kotlin', 'mps', 'phpstorm'
                // , 'pycharm-ce', 'pycharm-edu', 'pycharm', 'resharper', 'resharper-cpp'
                // , 'resharper', 'rider', 'rubymine', 'teamcity', 'test', 'toolbox', 'upsource'
                // , 'webstorm', 'youtrack' ]
                    .map(productId => {
                        return { id : productId
                               , name: productId + '.svg'
                               , path : './assets/' + productId + '.svg'
                               };
                    })
                    .map(waitForContent);
            Promise.all(assetPromises)
                   .then(files =>
                        files.map(
                            ({ content, name }) => {
                                assets.file(name, content, { binary: true }) }
                        )
                    )
                   .then(() => zip.generateAsync({type:"blob"}))
                   .then(content => new FileSaver(content, source.product + "_html5.zip"));
            // zip.generateAsync({type:"blob"})
            //     .then(function(content) {
            //         new FileSaver(content, "export.zip");
            //     });
        });
    });
}

const prepareImportExport = () => {
    app.ports.export_.subscribe((exportedState) => {
        const exportCode = export_(app, exportedState).json;

        document.getElementById('export-target').className = 'shown';
        document.getElementById('export-code').value = exportCode;
    });
    app.ports.exportZip_.subscribe((exportedState) => {
        try {
            // console.log('exportedState', exportedState);
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
                const importedScene = JSON.parse(document.getElementById('import-code').value);
                import_(app, importedScene);
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
    const srcCanvas = document.querySelector('.webgl-layers');
    const trgCanvas = document.querySelector('#js-save-buffer');
    const [ width, height ] = [ srcCanvas.width, srcCanvas.height ];
    trgCanvas.width = width;
    trgCanvas.height = height;
    if (!srcCanvas || !trgCanvas) return;
    trgCanvas.style.display = 'block';
    requestAnimationFrame(() => { // without that, image buffer will be empty
        const trgContext = trgCanvas('2d');
        trgContext.drawImage(srcCanvas, 0, 0);
        drawToCanvas.html(document.querySelector('.svg-layers'), trgCanvas, width, height, () => {
        // FIXME: a temporary hack to draw a logo on the canvas,
        // use product image itself instead
        hiddenLink.download = width + 'x'+ height + '-jetbrains.png';
            if (document.querySelector('.logo-layer')) {
                const logoSrc = document.querySelector('.logo-layer');
                const state = JSON.parse(logoSrc.getAttribute('data-stored'));
                drawToCanvas.image(state.logoPath,
                    function(image, context) {
                        context.translate(state.posX, state.posY);
                        context.scale(state.scale, state.scale);
                        context.globalCompositeOperation = state.blend;
                        image.width = state.width;
                        image.height = state.height;
                    },
                    trgCanvas, 0, 0, 120, 120,
                    () => {
                        trgCanvas.toBlob(blob => {
                            const url = URL.createObjectURL(blob);
                            hiddenLink.href = url;
                            hiddenLink.click();
                            URL.revokeObjectURL(url);
                            trgCanvas.style.display = 'none';
                        });
                    }
                );
            } else {
                trgCanvas.toBlob(blob => {
                    const url = URL.createObjectURL(blob);
                    hiddenLink.href = url;
                    hiddenLink.click();
                    URL.revokeObjectURL(url);
                    trgCanvas.style.display = 'none';
                });
            }
        });
    });
}

prepareImportExport();

// document.addEventListener('DOMContentLoaded', () => {
setTimeout(() => {

    const hiddenLink = document.createElement('a');
    hiddenLink.download = 'jetbrains-art-v2.png';

    app.ports.presetSizeChanged.subscribe(size => {
        if (savingBatch) {
            // console.log('saving ', size);
            savePng(hiddenLink, size);
        };
    });

    app.ports.startGui.subscribe((model) => {
        // console.log('startGui', model);
        model.layers.forEach(layer => {
            layer.model = JSON.parse(layer.model) || {};
        });
        startGui(
            document,
            model,
            { changeLightSpeed : index => value =>
                { app.ports.changeLightSpeed.send({ layer: index, value: Math.round(value) }) }
            , changeVignette : index => value =>
                { app.ports.changeVignette.send({ layer: index, value: value }) }
            , changeIris : index => value =>
                { app.ports.changeIris.send({ layer: index, value: value }) }
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
            , saveBatch : sizes_ => {
                let sizes = sizes_.concat([[0, 0]]);
                let sizeIndex = 0;
                savingBatch = true;
                const nextPng = () => {
                    if (sizeIndex < sizes.length) {
                        const [ width, height ] = sizes[sizeIndex];
                        // console.log('sending', width, height);
                        app.ports.setCustomSize.send([ width, height ]);
                        sizeIndex = sizeIndex + 1;
                        setTimeout(nextPng, batchPause);
                    } else {
                        savingBatch = false;
                        // console.log('done saving batch');
                    }
                };

                nextPng();
            }
            , changeAmplitude : index => (x, y, z) => {
                app.ports.changeAmplitude.send({ layer: index, value: [ x, y, z ]});
            }
            , shiftColor : index => (h, s, b) => {
                app.ports.shiftColor.send({ layer: index, value: [ h, s, b ]});
            }
            , turnOn : index =>
                { app.ports.turnOn.send(index); }
            , turnOff : index =>
                { app.ports.turnOff.send(index); }
            , mirrorOn : index =>
                { app.ports.mirrorOn.send(index); }
            , mirrorOff : index =>
                { app.ports.mirrorOff.send(index); }
            , rotate : value =>
                { app.ports.rotate.send(value); }
            , applyRandomizer : value =>
                { app.ports.applyRandomizer.send(prepareModelForImport(value)); }
            });

        model.layers.forEach((layer, index) => {
            if (isFss(layer)) {
                // console.log('rebuild FSS layer', index);
                const fssScene = buildFSS(model, layer.model);
                fssScenes[index] = fssScene;
                app.ports.rebuildFss.send({ value: fssScene, layer: index });
            }
        });

        app.ports.requestFssRebuild.subscribe(({ layer : index, model, value : fssModel }) => {
            const layer = model.layers[index];
            if (isFss(layer)) {
                // console.log('forced to rebuild FSS layer', index);
                // FIXME: just use layer.model instead of `fssModel`
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


