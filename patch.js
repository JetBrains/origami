// RPD framework initialization

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
console.log(Rpd);

var elmLorenz = null;

Rpd.renderNext('svg', document.getElementById('patch-target'),
                { style: 'ableton' });

var patch = Rpd.addPatch('Lorenz').resizeCanvas(800, 800);

var sigmaNode = patch.addNode('util/knob', 'Sigma').move(60, 10);
var betaNode = patch.addNode('util/knob', 'Beta').move(60, 100);
var rhoNode = patch.addNode('util/knob', 'Rho').move(60, 200);
var stepNode = patch.addNode('util/knob', 'Step').move(60, 300);

var lorenzNode = patch.addNode('core/basic', 'Lorenz', {
    inlets: {
        'sigma' : { type: 'util/number', 'default': 10 },
        'beta' : { type: 'util/number', 'default': 2.6 },
        'rho' : { type: 'util/number', 'default': 28 },
        'step' : { type: 'util/number', 'default': 0.005 }
    },
    process: function(inlets) {
        if (elmLorenz) {
            elmLorenz.ports.modify.send({
                sigma: inlets.sigma || 10,
                beta: inlets.beta || 26,
                rho: inlets.rho || 28,
                step: inlets.step || 0.005
            });
        };
    }
}).move(240, 10);

var sigmaInlet = lorenzNode.inlets['sigma'];
var betaInlet = lorenzNode.inlets['beta'];
var rhoInlet = lorenzNode.inlets['rho'];
var stepInlet = lorenzNode.inlets['step'];

sigmaNode.outlets['number'].connect(sigmaInlet);
betaNode.outlets['number'].connect(betaInlet);
rhoNode.outlets['number'].connect(rhoInlet);
stepNode.outlets['number'].connect(stepInlet);

module.exports = function(elmLorenzInstance) {
    elmLorenz = elmLorenzInstance;
}
