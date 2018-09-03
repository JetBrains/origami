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

const DEFAULT_FACES_BY_X = 12;
const DEFAULT_FACES_BY_Y = 15;

function parseQuery(query) {
    const params = {};
    query.substr(1).split('&').map(pair => {
        [key, value] = pair.split('=');
        params[key] = value;
    });
    return params;
}

function start(layers, updateLayers) {
    Rpd.renderNext('svg', document.getElementById('patch-target'),
                    { style: 'ableton' });

    var patch = Rpd.addPatch('Elmsfeuer').resizeCanvas(window.innerWidth, window.innerHeight);

    var layersNode = patch.addNode('jb/layers').move(80, 250);
    layersNode.inlets['count'].receive(layers.length);
    layersNode.inlets['colors'].receive(layers.map((layer) => {
        return  (layer.type === 'fss') || (layer.type === 'fss-mirror')
            ? [ layer.config.lights.ambient[1]
              , layer.config.lights.diffuse[1]
              ]
            : []
    }));
    if (window.location.search) {
        const parsedQuery = parseQuery(window.location.search);
        if (parsedQuery.blends) {
            layersNode.inlets['code'].receive(parsedQuery.blends);
        }
    }

    var paletteNode = patch.addNode('jb/palette').move(350, 250);

    var knobFacesX = patch.addNode('util/knob',
        { process: function(inlets) {
            const newFacesX =
                Math.floor(inlets.knob * (inlets.max - inlets.min))
                    || DEFAULT_FACES_BY_X;
            if (updateLayers) {
                updateLayers(function(prevConfig) {
                    prevConfig.faces = [ newFacesX, prevConfig.faces[1] ];
                    return prevConfig;
                });
            }
            return {};
          }
        }).move(360, 360);
    knobFacesX.inlets['max'].receive(40);
    var knobFacesY = patch.addNode('util/knob',
        { process: function(inlets) {
            const newFacesY =
                Math.floor(inlets.knob * (inlets.max - inlets.min))
                    || DEFAULT_FACES_BY_Y;
            if (updateLayers) {
                updateLayers(function(prevConfig) {
                    prevConfig.faces = [ prevConfig.faces[0], newFacesY ];
                    return prevConfig;
                });
            }
            return {};
        }
        }).move(360, 450);
    knobFacesY.inlets['max'].receive(40);

    return {
        layersNode: layersNode,
        paletteNode: paletteNode
    }
}

module.exports = start;
