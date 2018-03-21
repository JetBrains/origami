window.FSSAvoidFloat32Array = true;

require('./node_modules/flat-surface-shader/source/Core');
require('./node_modules/flat-surface-shader/source/Vector3');
require('./node_modules/flat-surface-shader/source/Vector4');
require('./node_modules/flat-surface-shader/source/Vertex');
require('./node_modules/flat-surface-shader/source/Color');
require('./node_modules/flat-surface-shader/source/Triangle');
require('./node_modules/flat-surface-shader/source/Object');
const Light = require('./node_modules/flat-surface-shader/source/Light');
const Material = require('./node_modules/flat-surface-shader/source/Material');
const Geometry = require('./node_modules/flat-surface-shader/source/Geometry');
const Plane = require('./node_modules/flat-surface-shader/source/Plane');
const Mesh = require('./node_modules/flat-surface-shader/source/Mesh');
const Scene = require('./node_modules/flat-surface-shader/source/Scene');

function startFss(port) {
    var scene = new FSS.Scene();
    var light = new FSS.Light('#111122', '#FF0022');
    //var geometry = new FSS.Plane(600, 400, 16, 8);
    var geometry = new FSS.Plane(1600, 2400, 16, 24);
    var material = new FSS.Material('#FFFF22', '#FF00FF');
    var mesh = new FSS.Mesh(geometry, material);
    var now, start = Date.now();

    function initialise() {
        scene.add(mesh);
        scene.add(light);
        light.setPosition(300*Math.sin(0.001), 200*Math.cos(0.0005), 60);
        //container.appendChild(renderer.element);
        //window.addEventListener('resize', resize);
    }

    function resize() {
        //renderer.setSize(container.offsetWidth, container.offsetHeight);
    }

    /*function animate() {
        now = Date.now() - start;
        light.setPosition(300*Math.sin(now*0.001), 200*Math.cos(now*0.0005), 60);
        //renderer.render(scene);
        requestAnimationFrame(animate);
    }*/

    initialise();
    //resize();
    //animate();

    console.log(scene);
    port.send(scene);
}

module.exports = startFss;
