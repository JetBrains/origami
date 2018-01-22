var d3 = require('d3');
var Kefir = require('Kefir');

window.Rpd = require('./node_modules/rpd/src/rpd.js');
console.log(Rpd);

var SVG_XMLNS = "http://www.w3.org/2000/svg";

var elmLorenz = null;

Rpd.nodetype('jb/layers', {
    title: 'Layers',
    inlets: {
        'count': { type: 'util/number', default: 3 }
    },
    outlets: {},
    process: function(inlets) { return {}; }
});

Rpd.noderenderer('jb/layers', 'svg', {
    size: { width: 500, height: 300 },
    pivot: { x: 0.03, y: 0.03 },
    first: function(bodyElm) {
        d3.select(bodyElm).append('text')
                            .text('Test')
                            .attr('data-test', '1');
    },
    always: function(bodyElm, inlets) {
        var layersCount = parseInt(inlets.count);
        console.log(layersCount);
        d3.select(bodyElm).selectAll("*").remove();
        for (var i = 0; i < layersCount; i++) {
            bodyElm.appendChild(createLayerBlendControls(i).node());
        }
        console.log(layersCount);
        //lastForms = inlets.forms;
        //if (lastForms && lastForms.length)
        //myP5.redraw();
    }
});

eqFuncMap = [ '+', '-', 'R+' ];
eqFactorMap = [
    '0',    // 0, B.zero
    '1',    // 1, B.one
    'SC',   // 2, B.srcColor
    '1-SC', // 3, B.oneMinusSrcColor
    'DC',   // 4, B.dstColor
    '1-DC', // 5, B.oneMinusDstColor
    'SA',   // 6, B.srcAlpha
    '1-SA', // 7, B.oneMinusSrcAlpha
    'DA',   // 8, B.dstAlpha
    '1-DA', // 9, B.oneMinusDstAlpha
    'SAS',  // 10, B.srcAlphaSaturate
    'CC',   // 11, B.constantColor
    '1-CC', // 12, B.oneMinusConstantColor
    'CA',   // 13, B.constantAlpha
    '1-CA'  // 14, B.oneMinusConstantAlpha
]

// 0 -> B.zero
// 1 -> B.one
// 2 -> B.srcColor
// 3 -> B.oneMinusSrcColor
// 4 -> B.dstColor
// 5 -> B.oneMinusDstColor
// 6 -> B.srcAlpha
// 7 -> B.oneMinusSrcAlpha
// 8 -> B.dstAlpha
// 9 -> B.oneMinusDstAlpha
// 10 -> B.srcAlphaSaturate
// 11 -> B.constantColor
// 12 -> B.oneMinusConstantColor
// 13 -> B.constantAlpha
// 14 -> B.oneMinusConstantAlpha
// _ -> B.zero)

function createLayerBlendControls(layerId) {
    var root = d3.select(document.createElementNS(SVG_XMLNS, 'g')).attr('data-layer', layerId)
                 .attr('transform', 'translate(0, ' + (layerId * 100) + ')');
    var state = {
        layer: layerId,
        blend: {
            r: 0, g: 0, b: 0, a: 0,
            colorEq: [ 0, 1, 0 ], // ( 0..3, 0..15, 0..15 )
            alphaEq: [ 0, 1, 0 ]
        }
    }
    root.append('g')
        .call(function(colorRoot) {
            colorRoot.append('g')
                .call(function(funcRoot) {
                    eqFuncMap.map(function(funcName, i) {
                        funcRoot.append('text').attr('fill', 'white').style('cursor', 'pointer')
                                .attr('transform', 'translate(' + (i * 30) + ',0)')
                                .text(funcName);
                    });
                });
            colorRoot.append('g')
                .attr('transform', 'translate(0,12)')
                .call(function(factor1Root) {
                    eqFactorMap.map(function(factorName, i) {
                        factor1Root.append('text').attr('fill', 'white').style('cursor', 'pointer')
                                   .attr('transform', 'translate(' + (i * 30) + ',0)')
                                   .text(factorName);
                    });
                });
            colorRoot.append('g')
                .attr('transform', 'translate(0,24)')
                .call(function(factor2Root) {
                    eqFactorMap.map(function(factorName, i) {
                        factor2Root.append('text').attr('fill', 'white').style('cursor', 'pointer')
                                   .attr('transform', 'translate(' + (i * 30) + ',0)')
                                   .text(factorName);
                    });
                });
        });
    root.append('g')
        .attr('transform', 'translate(0,40)')
        .call(function(alphaRoot) {
            alphaRoot.append('g')
                .call(function(funcRoot) {
                    eqFuncMap.map(function(funcName, i) {
                        funcRoot.append('text').attr('fill', 'white').style('cursor', 'pointer')
                                .attr('transform', 'translate(' + (i * 30) + ',0)')
                                .text(funcName);
                    });
                });
            alphaRoot.append('g')
                .attr('transform', 'translate(0,12)')
                .call(function(factor1Root) {
                    eqFactorMap.map(function(factorName, i) {
                        factor1Root.append('text').attr('fill', 'white').style('cursor', 'pointer')
                                .attr('transform', 'translate(' + (i * 30) + ',0)')
                                .text(factorName);
                    });
                });
            alphaRoot.append('g')
                .attr('transform', 'translate(0,24)')
                .call(function(factor2Root) {
                    eqFactorMap.map(function(factorName, i) {
                        factor2Root.append('text').attr('fill', 'white').style('cursor', 'pointer')
                                .attr('transform', 'translate(' + (i * 30) + ',0)')
                                .text(factorName);
                    });
                });
        });
    return root;
}

module.exports = function(elmLorenzInstance) {
    elmLorenz = elmLorenzInstance;
}
