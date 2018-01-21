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
sigmaNode.inlets['min'].receive(0);
sigmaNode.inlets['max'].receive(100);
var betaNode = patch.addNode('util/knob', 'Beta').move(60, 100);
betaNode.inlets['min'].receive(0);
betaNode.inlets['max'].receive(15);
var rhoNode = patch.addNode('util/knob', 'Rho').move(60, 200);
rhoNode.inlets['min'].receive(0);
rhoNode.inlets['max'].receive(100);
var stepNode = patch.addNode('util/knob', 'Step').move(60, 300);
stepNode.inlets['min'].receive(0);
stepNode.inlets['max'].receive(0.1);
var thicknessNode = patch.addNode('util/knob', 'Thickness').move(60, 400);
thicknessNode.inlets['min'].receive(0.1);
thicknessNode.inlets['max'].receive(2);
var numVerticesNode = patch.addNode('util/knob', 'NumVertices').move(60, 500);
numVerticesNode.inlets['min'].receive(100);
numVerticesNode.inlets['max'].receive(2000);

var lorenzNode = patch.addNode('core/basic', 'Lorenz', {
    inlets: {
        'sigma' : { type: 'util/number', 'default': 10 },
        'beta' : { type: 'util/number', 'default': 2.6 },
        'rho' : { type: 'util/number', 'default': 28 },
        'step' : { type: 'util/number', 'default': 0.005 },
        'thickness': { type: 'util/number', 'default': 1 },
        'numVertices': { type: 'util/number', 'default': 2000 }
    },
    process: function(inlets) {
        if (elmLorenz) {
            elmLorenz.ports.modify.send({
                sigma: inlets.sigma || 10,
                beta: inlets.beta || 2.6,
                rho: inlets.rho || 28,
                step: inlets.step || 0.005,
                thickness: inlets.thickness || 1.0,
                numVertices : Math.floor(inlets.numVertices) || 2000
            });
        };
    }
}).move(240, 10);

patch.addNode('jb/layers').move(60, 600);

var sigmaInlet = lorenzNode.inlets['sigma'];
var betaInlet = lorenzNode.inlets['beta'];
var rhoInlet = lorenzNode.inlets['rho'];
var stepInlet = lorenzNode.inlets['step'];
var thicknessInlet = lorenzNode.inlets['thickness'];
var numVerticesInlet = lorenzNode.inlets['numVertices'];

sigmaNode.outlets['number'].connect(sigmaInlet);
betaNode.outlets['number'].connect(betaInlet);
rhoNode.outlets['number'].connect(rhoInlet);
stepNode.outlets['number'].connect(stepInlet);
thicknessNode.outlets['number'].connect(thicknessInlet);
numVerticesNode.outlets['number'].connect(numVerticesInlet);

sigmaInlet.receive(10);
betaInlet.receive(2.6);
rhoInlet.receive(28);
stepInlet.receive(0.005);
thicknessInlet.receive(1.0);
numVerticesInlet.receive(2000);

module.exports = function(elmLorenzInstance) {
    elmLorenz = elmLorenzInstance;
}
