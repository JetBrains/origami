window.Kefir = require('kefir');
window.d3 = require('d3-selection');

require('./node_modules/rpd/src/render/svg.css');
require('./node_modules/rpd/src/render/shared.js');
require('./node_modules/rpd/src/render/svg.js');

require('./node_modules/rpd/src/style/ableton/svg.css');
require('./node_modules/rpd/src/style/ableton/svg.js');

window.RpdUtils = require('./node_modules/rpd/src/toolkit/util/shared.js');
require('./node_modules/rpd/src/toolkit/util/toolkit.js');
require('./node_modules/rpd/src/toolkit/util/svg.js');

window.Rpd = require('./node_modules/rpd/src/rpd.js');

function parseQuery(query) {
    const params = {};
    query.substr(1).split('&').map(pair => {
        [key, value] = pair.split('=');
        params[key] = value;
    });
    return params;
}

function start(layers) {
    Rpd.renderNext('svg', document.getElementById('patch-target'),
                    { style: 'ableton' });

    var patch = Rpd.addPatch('Elmsfeuer').resizeCanvas(window.innerWidth, window.innerHeight);

    var layersNode = patch.addNode('jb/layers').move(80, 250);
    layersNode.inlets['count'].receive(layers.length);
    layersNode.inlets['colors'].receive(layers.map((layer) => layer.config.colors));
    if (window.location.search) {
        const parsedQuery = parseQuery(window.location.search);
        if (parsedQuery.blends) {
            layersNode.inlets['code'].receive(parsedQuery.blends);
        }
    }

    var paletteNode = patch.addNode('jb/palette').move(350, 250);
}

module.exports = start;
