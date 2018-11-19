const buildFSS = require('./fss.js');
const deepClone = require('./deep-clone.js')
const App = require('./src/Main.elm');

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const import_ = (app, importedState) => {

    const parsedState = importedState;

    app.ports.requestFssRebuild.subscribe(({ layer : index, model, value : fssModel }) => {
        const layer = model.layers[index];
        if (isFss(layer)) {
            //console.log('forced to rebuild FSS layer', index);
            // FIXME: just use layer.model instead of `fssModel`
            const fssScene = buildFSS(model, fssModel, parsedState.layers[index].sceneFuzz);
            app.ports.rebuildFss.send({ value: fssScene, layer: index });
        }

        app.ports.hideControls.send(null);
       // app.ports.pause.send(null); TODO: control by url parameter
    });

    const toSend = deepClone(parsedState);
    toSend.layers =
        parsedState.layers.map(layer => {
            const layerModel = deepClone(layer);
            layerModel.model = JSON.stringify(layer.model);
            return layerModel;
        });
    //console.log('sending for the import', toSend);

    app.ports.import_.send(JSON.stringify(toSend));
}

window.runGenScene = function() {
    var node = document.getElementById("app");
    var app = App.Main.embed(node);

    //console.log('runGenScene', window.jsGenScene, app);

    import_(app, window.jsGenScene);
}
