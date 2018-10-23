const buildFSS = require('./fss.js');
const App = require('./src/Main.elm');

const import_ = (app, importedState) => {
    //const parsedState = JSON.parse(importedState);
    //debugger;

    const parsedState = importedState;
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
        origin: parsedState.origin,
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
        if (layer.type == 'fss') {
            const scene = buildFSS(layer.config, layer.sceneFuzz);
            scenes[index] = scene;
            console.log('import FSS', scene, layer.config);
            app.ports.configureFss.send([ layer.config, index ]);
            app.ports.rebuildFss.send([ scene, index ]);
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
