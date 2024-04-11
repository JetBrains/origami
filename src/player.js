const buildFSS = require('./fss.js');
const isFss = require('./is-fss.js');
const deepClone = require('./deep-clone.js')
const App = require('./elm_src/Main.elm');

const import_ = (app, importedState) => {

    document.body.style.backgroundColor = importedState.background;

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

const runGenScene = () => {
    const node = document.getElementById("app");
    const app = App.Elm.Main.init({ node: node });

    //console.log('runGenScene', window.jsGenScene, app);

    app.ports.requestFitToWindow.subscribe((_) => {
        app.ports.resize.send(
            { presetCode: null, viewport: [ window.innerWidth, window.innerHeight ]}
        );
    });

    import_(app, window.jsGenScene);
}

runGenScene();
