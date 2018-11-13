const buildFSS = require('./fss.js');
const App = require('./src/Main.elm');

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const import_ = (app, importedState) => {
    //const parsedState = JSON.parse(importedState);
    //debugger;

    const parsedState = importedState;
    // scenes = {};
    // layers = [];

    app.ports.requestFssRebuild.subscribe(({ layer : index, model, value : fssModel }) => {
        const layer = model.layers[index];
        if (isFss(layer)) {
            console.log('forced to rebuild FSS layer', index);
            const fssScene = buildFSS(model, fssModel, parsedState.layers[index].sceneFuzz);
            // scenes[index] = fssScene;
            app.ports.rebuildFss.send({ value: fssScene, layer: index });
            // layer.scene = fssScene;
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

    // app.ports.hideControls.send(null);
    // app.ports.pause.send(null);

    // app.ports.initLayers.send(layers.map((l) => l.kind));

    // model.layers.forEach((layer, index) => {
    //     if (isFss(layer)) {
    //         console.log('rebuild FSS layer', index);
    //         const fssScene = buildFSS(model, model.fss);
    //         app.ports.rebuildFss.send({ value: fssScene, layer: index });
    //     }
    // });

    // parsedState.layers.forEach((layer, index) => {
    //     if (isFss(layer)) {
    //         const scene = buildFSS(parsedState, layer.model, layer.sceneFuzz);
    //         scenes[index] = scene;
    //         console.log('import FSS', scene, parsedState);
    //         //app.ports.configureFss.send({ value: layer.model, layer: index });
    //         app.ports.rebuildFss.send({ value: scene, layer: index });
    //     }
    // });

    // const mergedBlends = parsedState.layers.map(layer => layer.blend).join(':');
    // window.location.hash = '#blends=' + mergedBlends;

    //app.ports.continue.send(null);
    //if (layersNode) layersNode.inlets['code'].receive(mergedBlends);
}

window.runGenScene = function() {
    var node = document.getElementById("app");
    var app = App.Main.embed(node);

    console.log('runGenScene', window.jsGenScene, app);

    import_(app, window.jsGenScene);
}
