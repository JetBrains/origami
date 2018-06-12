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
            color : { r: 0, g: 0, b: 0, a: 0 },
            colorEq: [ 0, 1, 0 ], // ( 0..3, 0..15, 0..15 )
            alphaEq: [ 0, 1, 0 ]  // ( 0..3, 0..15, 0..15 )
        }
    }

    root.append('g').attr('data-layer-blend', 'color')
        .call(function(colorRoot) {
            colorRoot.append('g').attr('data-blend', 'eq')
                     .call(mapTextOver(eqFuncMap, 30, eqFuncClick('color', state)));
            colorRoot.append('g').attr('data-blend', 'factor1')
                     .attr('transform', 'translate(0,12)')
                     .call(mapTextOver(eqFactorMap, 30, eqFactorClick('color', 1, state)));
            colorRoot.append('g').attr('data-blend', 'factor2')
                     .attr('transform', 'translate(0,24)')
                     .call(mapTextOver(eqFactorMap, 30, eqFactorClick('color', 2, state)));
        });

    root.append('g').attr('data-layer-blend', 'alpha')
        .attr('transform', 'translate(0,40)')
        .call(function(alphaRoot) {
            alphaRoot.append('g').attr('data-blend', 'eq')
                     .call(mapTextOver(eqFuncMap, 30, eqFuncClick('alpha', state)));
            alphaRoot.append('g').attr('data-blend', 'factor1')
                     .attr('transform', 'translate(0,12)')
                     .call(mapTextOver(eqFactorMap, 30, eqFactorClick('alpha', 1, state)));
            alphaRoot.append('g').attr('data-blend', 'factor2')
                     .attr('transform', 'translate(0,24)')
                     .call(mapTextOver(eqFactorMap, 30, eqFactorClick('alpha', 2, state)));
        });

    return root;
}

function eqFuncClick(blendType, state) {
    return function(node, index) {
        return function() {
            state.blend[blendType + 'Eq'][0] = index;
            // console.log(state);
            if (elmsfeuer) elmsfeuer.ports.changeBlend.send(state);
        }
    }
}

function eqFactorClick(blendType, factorId, state) {
    return function(node, index) {
        return function() {
            state.blend[blendType + 'Eq'][factorId] = index;
            // console.log(state);
            if (elmsfeuer) elmsfeuer.ports.changeBlend.send(state);
        }
    }
}

function mapTextOver(list, shiftX, onClick) {
    return function(root) {
        list.map(function(text, i) {
            var textNode =
                root.append('text').attr('fill', 'white').style('cursor', 'pointer')
                    .attr('transform', 'translate(' + (i * shiftX) + ',0)')
                    .text(text).node();
            Kefir.fromEvents(textNode, 'click').onValue(onClick(textNode, i));
        });
    }
}
