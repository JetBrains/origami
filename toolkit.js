var d3 = require('d3');
var Kefir = require('Kefir');

window.Rpd = require('./node_modules/rpd/src/rpd.js');
// console.log(Rpd);

var SVG_XMLNS = "http://www.w3.org/2000/svg";

var elmsfeuer = null;
var layersNodeApp = null;
var layersNode = null;

Rpd.nodetype('jb/layers', {
    title: 'Layers',
    inlets: {
        'count': { type: 'util/number', default: 3 }
    },
    outlets: {},
    process: function(inlets) {
        if (layersNode) layersNode.ports.changeLayerCount.send(parseInt(inlets.count));
        return {};
    }
});

Rpd.noderenderer('jb/layers', 'svg', {
    size: { width: 500, height: 400 },
    pivot: { x: 0.03, y: 0.03 },
    first: function(bodyElm) {
        // console.log(layersNode);
        if (layersNodeApp) {
            layersNode = layersNodeApp.embed(bodyElm);
            layersNode.ports.resize.send([ 500, 400 ]);
            if (elmsfeuer) {
                layersNode.ports.sendNewBlend.subscribe(function(state) {
                    elmsfeuer.ports.changeBlend.send(state);
                })
            }
        }
    },
    always: function(bodyElm, inlets) {
        var layersCount = parseInt(inlets.count);
        if (layersNode) {
            layersNode.ports.changeLayerCount.send(layersCount);
        }
    }
});

module.exports = function(elmsfeuerInstance, layersNodeApp_) {
    elmsfeuer = elmsfeuerInstance;
    layersNodeApp = layersNodeApp_;
};
