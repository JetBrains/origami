const buildFSS = require('./fss.js');
const App = require('./src/Main.elm');

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const import_ = (app, importedState) => {

    const parsedState = importedState;

    app.ports.requestFssRebuild.subscribe(({ layer : index, model, value : fssModel }) => {
        const layer = model.layers[index];
        if (isFss(layer)) {
            console.log('forced to rebuild FSS layer', index);
            // FIXME: just use layer.model instead of `fssModel`
            const fssScene = buildFSS(model, fssModel, parsedState.layers[index].sceneFuzz);
            app.ports.rebuildFss.send({ value: fssScene, layer: index });
        }

        app.ports.hideControls.send(null);
        app.ports.pause.send(null);
    });

    console.log('sending for the import', parsedState);
    app.ports.import_.send(JSON.stringify({
        theta: parsedState.theta,
        omega: parsedState.omega,
        size: parsedState.size,
        origin: parsedState.origin,
        mouse: parsedState.mouse,
        now: parsedState.now,
        product: parsedState.product,
        layers: parsedState.layers.map((layer) => (
            { kind : layer.kind,
              blend: layer.blend,
              isOn: layer.isOn,
              model: JSON.stringify(layer.model),
              name: layer.name
            }
        ))
    }));
}

window.runGenScene = function() {
    var node = document.getElementById("app");
    var app = App.Main.embed(node);

    console.log('runGenScene', window.jsGenScene, app);

    import_(app, window.jsGenScene);
}
