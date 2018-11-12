const buildFSS = require('./fss.js');
const App = require('./src/Main.elm');

const import_ = (app, importedState) => {
    //const parsedState = JSON.parse(importedState);
    //debugger;

    const parsedState = importedState;
    scenes = {};
    layers = [];
    app.ports.hideControls.send(null);
    app.ports.pause.send(null);
    app.ports.initLayers.send(layers.map((l) => l.kind));
    app.ports.import_.send(JSON.stringify({
        theta: parsedState.theta,
        size: parsedState.size,
        origin: parsedState.origin,
        mouse: parsedState.mouse,
        now: parsedState.now,
        layers: parsedState.layers.map((layer) => (
            { kind : layer.kind,
              blend: layer.blend,
              isOn: layer.isOn,
              model: layer.model
            }
        ))
    }));
    parsedState.layers.forEach((layer, index) => {
        if (layer.kind == 'fss' || layer.kind == 'fss-mirror')  {
            const scene = buildFSS(parsedState, layer.model, layer.sceneFuzz);
            scenes[index] = scene;
            console.log('import FSS', scene, parsedState);
            app.ports.configureFss.send({ value: layer.model, layer: index });
            app.ports.rebuildFss.send({ value: scene, layer: index });
        }
    });
    const mergedBlends = parsedState.layers.map(layer => layer.blend).join(':');
    window.location.hash = '#blends=' + mergedBlends;
    //app.ports.continue.send(null);
    //if (layersNode) layersNode.inlets['code'].receive(mergedBlends);
}

window.runGenScene = function() {
    var node = document.getElementById("app");
    var app = App.Main.embed(node);

    console.log('runGenScene', window.jsGenScene, app);

    import_(app, window.jsGenScene);
}
