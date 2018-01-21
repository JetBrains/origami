var d3 = require('d3');
var Kefir = require('Kefir');

window.Rpd = require('./node_modules/rpd/src/rpd.js');
console.log(Rpd);

Rpd.nodetype('jb/layers', {
    title: 'Layers',
    inlets: {},
    outlets: {},
    process: function(inlets) { return {}; }
});

Rpd.noderenderer('jb/layers', 'svg', {
    size: { width: 55, height: 30 },
    /* pivot: { x: 0, y: 0 }, */
    first: function(bodyElm) {
        console.log('test');
        d3.select(bodyElm).append('text')
                            .text('Test')
                            .attr('data-test', '1');
    },
    always: function(bodyElm, inlets) {
        //lastForms = inlets.forms;
        //if (lastForms && lastForms.length)
        //myP5.redraw();
    }
});
