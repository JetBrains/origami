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
const DEFAULT_LIGHT_SPEED = 400;

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
    if (window.location.hash && (window.location.hash.indexOf('#blends=' > 0))) {
        const newBlends = window.location.hash.slice(8);
        layersNode.inlets['code'].receive(newBlends);
    } else {
        const codes = layers.map((layer, layerIdx) => {
            if (layer.type == 'text') {
                return '_normal';
            } else {
                return '00000000010010'; // FIXME: a dirty way to set default value
            }
        });
        layersNode.inlets['code'].receive(codes.join(':'));
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

    knobFacesX.inlets['max'].receive(140);

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
        }).move(60, 250);
    knobFacesY.inlets['max'].receive(140);

    var knobLightSpeed = patch.addNode('util/knob',
        { title: 'Speed',
          process: function(inlets) {
                const newLightSpeed =
                    Math.floor(inlets.knob * (inlets.max - inlets.min))
                    || DEFAULT_LIGHT_SPEED;
                if (updateLayers) {
                    updateLayers(function(prevConfig) {
                        prevConfig.lights.speed = newLightSpeed;
                        // prevConfig.lights.forEach(light => {
                        //     light.speed = newLightSpeed;
                        // });
                        return prevConfig;
                    });
                }
                return {};
            }
        }).move(160, 350);
    knobLightSpeed.inlets['max'].receive(1140);

    return {
        layersNode: layersNode,
        paletteNode: paletteNode
    }
}

module.exports = start;
