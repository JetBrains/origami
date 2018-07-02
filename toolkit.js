var d3 = require('d3');
//var Kefir = require('Kefir');

window.Kefir = require('kefir');
window.Rpd = require('./node_modules/rpd/src/rpd.js');
// console.log(Rpd);

var SVG_XMLNS = "http://www.w3.org/2000/svg";

var elmsfeuer = null;
var layersNodeApp = null;
var layersNode = null;
var updateFssColors = null;

function howMuch(single, plural) {
    return function(list) {
        if (!list) return 'Nothing';
        if (list.length == 0) return 'No ' + plural;
        if (list.length == 1) return 'One ' + single;
        if (list.length == 2) return 'Two ' + plural;
        return list.length + ' ' + plural;
    };
}

var PRODUCTS = [
    { label: 'JB', id: 'jetbrains',
      palette: [ '#9151e1',  '#ec4476', '#fde74a']
    },  // jetbrains-1
    { label: 'D1', id: 'default1',
      palette: [ '#f45b69',  '#e4fde1', 'rgba(0,0,0,0)']
    },  // default-1
    { label: 'D2', id: 'default2',
      palette: [ '#4b4e76',  '#fb4e76', 'rgba(0,0,0,0)']
    },  // default-2
    { label: ' ',  id: 'empty0', palette: [ ]
    },  // separator
    { label: 'IJ',
      id: 'intellij-idea',
      palette: [ '#087cfa', '#fe315d', '#f97a12' ]
    },  // idea // IJ_
    { label: 'PS', id: 'phpstorm',
      palette: [ '#b24eee', '#7660f4', '#fc378c' ]
    },  // phpstorm // PS_
    { label: 'PC',
      id: 'pycharm',
      palette: [ '#21d789', '#fcf84a', '#07c3f2' ]
    },  // pycharm // PC_
    { label: 'RM', id: 'rubymine',
      palette: [ '#fc2555', '#fd8638', '#8f41cd' ]
    },  // rubymine // RM_
    { label: 'WS', id: 'webstorm',
      palette: [ '#22cdd6', '#2888d4', '#feee56' ]
    },  // webstorm // WS_
    { label: 'CL', id: 'clion',
      palette: [ '#32d791', '#1a9edd', '#ea3a8c' ]
    },  // clion // CL_
    { label: 'DG', id: 'datagrip',
      palette: [ '#32d791', '#9779f5', '#fd5fe4' ]
    },  // // DG_
    { label: 'AC', id: 'appcode',
      palette: [ '#2b7fe3', '#25daee', '#30de95' ]
    },  // appcode // AC_
    { label: 'GO', id: 'gogland',
      palette: [ '#078efc', '#bb4efc', '#3bea62' ]
    },  // gogland // GO_
    { label: ' ', id: 'empty1', palette: [ ]
    },  // separator
    { label: 'R#', id: 'resharper',
      palette: [ '#c21456', '#e14ce3', '#fdbc2c' ]
    },  // resharper // R#_
    { label: 'R++', id: 'resharper-cpp',
      palette: [ '#fdbc2c', '#e14ce3', '#c21456' ]
    },  // // R++_
    { label: 'DC', id: 'dotcover',
      palette: [ '#fd7522', '#786bfb', '#e14ce3' ]
    },  // dotcover // DC_
    { label: 'DM', id: 'dotmemory',
      palette: [ '#fdbc2c', '#786bfb', '#e14ce3' ]
    },  // // DM_
    { label: 'DP', id: 'dotpeek',
      palette: [ '#23cbfc', '#786bfb', '#e14ce3' ]
    },  // // DP_
    { label: 'DT', id: 'dottrace',
      palette: [ '#fc1681', '#786bfb', '#e14ce3' ]
    },  // dottrace // DT_
    { label: 'RD', id: 'rider',
      palette: [ '#c40b55', '#e800ca', '#ffbd00' ]
    },  // rider // RD_
    { label: ' ', id: 'empty2',
      palette: [ ]
    },  // separator
    { label: 'TC', id: 'teamcity',
      palette: [ '#22b1ef', '#9062f7', '#46e869' ]
    },  // teamcity // TC_
    { label: 'YT', id: 'youtrack',
      palette: [ '#22b1ef', '#9062f7', '#fc378c' ]
    },  // youtrack // YT_
    { label: 'UP', id: 'upsource',
      palette: [ '#22b1ef', '#9062f7', '#fd8224' ]
    },  // upsource // UP_
    { label: 'HB', id: 'hub',
      palette: [ '#1fb9ee', '#965ff7', '#feec56' ]
    },  // hub // HB_
    { label: ' ', id: 'empty3', palette: [ ]
    },  // separator
    { label: 'KT', id: 'kotlin',
      palette: [ '#1b84f2', '#24dea7', '#ed4baa' ]
    },   // kotlin // KT_
    { label: 'MPS', id: 'mps',
      palette: [ '#31d68b', '#3188cd', '#f1e969' ]
    }  // mps // MPS_

];

Rpd.channeltype('jb/product', { });
Rpd.channeltype('jb/palette', { show: howMuch('color', 'colors') });

Rpd.nodetype('jb/layers', {
    title: 'Layers',
    inlets: {
        'count': { type: 'util/number', default: 3 },
        //'colors': { type: 'jb/colors', default: [] }
        'colors': { type: 'core/any', default: [] },
        'code': { type: 'core/any', default: '' }
    },
    outlets: {},
    process: function(inlets) {
        if (layersNode) {
            layersNode.ports.changeLayerCount.send(parseInt(inlets.count));
            if (inlets.colors) layersNode.ports.applyColors.send(inlets.colors);
        }
        return {};
    }
});

Rpd.noderenderer('jb/layers', 'svg', {
    size: { width: 212, height: 200 },
    pivot: { x: 0.03, y: 0.03 },
    first: function(bodyElm) {
        // console.log(layersNode);
        if (layersNodeApp) {
            layersNode = layersNodeApp.embed(bodyElm);
            layersNode.ports.resize.send([ 500, 400 ]);
            if (elmsfeuer) {
                layersNode.ports.sendNewBlend.subscribe(function(state) {
                    elmsfeuer.ports.changeBlend.send(state);
                });
                layersNode.ports.sendNewCode.subscribe(function(code) {
                    window.location.hash = '#blends=' + code;
                });
                layersNode.ports.sendNewColors.subscribe(function(state) {
                    // FIXME: remove the handler in palette node and use this one
                    //        for updates
                    if (updateFssColors) {
                        updateFssColors(state.layer, state.colors);
                    }
                });
            }
        }
    },
    always: function(bodyElm, inlets) {
        var layersCount = parseInt(inlets.count);
        if (layersNode) {
            layersNode.ports.changeLayerCount.send(layersCount);
            if (inlets.code) layersNode.ports.applyAllBlends.send(inlets.code);
        }
    }
});

Rpd.nodetype('jb/palette', {
    title: 'Product Palette',
    inlets: {
        'palette': { type: 'jb/palette', default: PRODUCTS[0].palette, label: 'selection', hidden: true },
        'product': { type: 'jb/product', default: PRODUCTS[0].id, label: 'product', hidden: true },
    },
    outlets: {
        'palette': { type: 'jb/palette' },
        'product': { type: 'jb/product' }
    },
    process: function(inlets) {
        // FIXME: remove the handler in this node and use the one in layers node
        //        instead
        if (updateFssColors && inlets.palette) {
            updateFssColors(0, inlets.palette.slice(0, 2));
            updateFssColors(1, inlets.palette.slice(2, 3).concat('#fb4e76'));
        }
        return {
            palette: inlets.palette,
            product: inlets.product
        };
    }
});

var PALETTE_NODE_WIDTH = PRODUCTS.length * 13;
var PALETTE_NODE_HEIGHT = 70;

var PALETTE_NODE_BODY_X = -(PALETTE_NODE_WIDTH / 2) - 45;
var PALETTE_NODE_BODY_Y = 5;
var LABEL_Y_SHIFT = 15;
Rpd.noderenderer('jb/palette', 'svg', function() {
    var cellSide = 12;
    return {
        size: { width: PALETTE_NODE_WIDTH, height: PALETTE_NODE_HEIGHT },
        first: function(bodyElm) {
            var paletteChange = Kefir.emitter();
            var productChange = Kefir.emitter();
            var lastSelected, lastHilitedLabel;
            var paletteGroups = [], labelText = {};
            d3.select(bodyElm)
                .append('g')
                .call(function(rootGroup) {
                    var labels = rootGroup.append('g')
                                          .attr('transform', 'translate(' + PALETTE_NODE_BODY_X +
                                                                     ', ' + ((-1 * PALETTE_NODE_HEIGHT / 2) + LABEL_Y_SHIFT) + ')');
                    PRODUCTS.forEach(function(product, i) {
                        labelText[product.id] =
                                    labels.append('text')
                                          .attr('class', 'rpd-jb-product-label')
                                          .attr('transform', 'translate(' + (i * 14) + ',  0)')
                                          .text(product.label);
                    });
                })
                .call(function(rootGroup) {
                    var palettes = rootGroup.append('g')
                                            .attr('transform', 'translate(' + PALETTE_NODE_BODY_X +
                                                                       ', ' + PALETTE_NODE_BODY_Y + ')');
                    PRODUCTS.forEach(function(product, i) {
                        var palette = product.palette;
                        palettes.append('g')
                                .attr('class', 'rpd-jb-palette-variant')
                                .attr('transform', 'translate(' + (i * 14) + ', ' +
                                                                (-1 * (palette.length / 2 * cellSide)) + ')')
                                .call((function(palette, productId) { return function(paletteGroup) {
                                    palette.forEach(function(color, i) {
                                        paletteGroup.append('rect').attr('rx', 4)
                                                    .attr('x', 0).attr('y', i * cellSide)
                                                    .attr('width', cellSide).attr('height', cellSide)
                                                    .attr('fill', color);
                                    });
                                    Kefir.fromEvents(paletteGroup.node(), 'click')
                                         .onValue(function() {
                                        if (lastSelected) lastSelected.attr('class', 'rpd-jb-palette-variant');
                                        if (lastHilitedLabel) lastHilitedLabel.attr('class', 'rpd-jb-product-label');
                                        labelText[productId].attr('class', 'rpd-jb-product-label rpd-jb-active-label');
                                        paletteGroup.attr('class', 'rpd-jb-palette-variant rpd-jb-active-variant');
                                        lastSelected = paletteGroup;
                                        lastHilitedLabel = labelText[productId];
                                        paletteChange.emit(palette);
                                        productChange.emit(product.id);
                                    });
                                    paletteGroups.push(paletteGroup);
                                } })(palette, product.id));
                    });
                });

            lastSelected = paletteGroups[0];
            paletteGroups[0].attr('class', 'rpd-jb-palette-variant rpd-jb-active-variant');
            labelText[PRODUCTS[0].id].attr('class', 'rpd-jb-product-label rpd-jb-active-label');
            lastHilitedLabel = labelText[PRODUCTS[0].id];
            return { 'palette': { valueOut: paletteChange },
                     'product':  { valueOut: productChange } };
        }
    };
});

function svgNode(name) {
    return document.createElementNS(SVG_XMLNS, name);
}

module.exports = function(elmsfeuerInstance, layersNodeApp_, updateFssColors_) {
    elmsfeuer = elmsfeuerInstance;
    layersNodeApp = layersNodeApp_;
    updateFssColors = updateFssColors_;
};
