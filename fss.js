window.FSSAvoidFloat32Array = true;

require('./node_modules/flat-surface-shader/source/Core');
require('./node_modules/flat-surface-shader/source/Vector3');
require('./node_modules/flat-surface-shader/source/Vector4');
require('./node_modules/flat-surface-shader/source/Vertex');
require('./node_modules/flat-surface-shader/source/Color');
require('./node_modules/flat-surface-shader/source/Triangle');
require('./node_modules/flat-surface-shader/source/Object');
require('./node_modules/flat-surface-shader/source/Light');
require('./node_modules/flat-surface-shader/source/Material');
require('./node_modules/flat-surface-shader/source/Geometry');
require('./node_modules/flat-surface-shader/source/Plane');
require('./node_modules/flat-surface-shader/source/Mesh');
require('./node_modules/flat-surface-shader/source/Scene');
require('./node_modules/flat-surface-shader/source/Math');

function buildFSS(config, source) {
    var scene = new FSS.Scene();


    var geometry = new FSS.Plane(config.size[0], config.size[1],
                                 config.faces[0], config.faces[1]);
    var material = new FSS.Material('#FFFFFF', '#FFFFFF');
    var mesh = new FSS.Mesh(geometry, material);

    var light = new FSS.Light('#000000', config.colors[0]);
    var highlight = new FSS.Light('#000000', config.colors[1]);

    function initialise() {
        scene.add(mesh);
        scene.add(light);
        scene.add(highlight);


        var v, vertex;
        for (v = geometry.vertices.length - 1; v >= 0; v--) {
            vertex = geometry.vertices[v];
            vertex.anchor = FSS.Vector3.clone(vertex.position);
            vertex.v0 = source && source[v]
                ? source[v].v0
                : FSS.Vector3.create(
                    Math.randomInRange(0.2, 1.0),
                    Math.randomInRange(0.2, 1.0),
                    Math.randomInRange(0.2, 1.0)
                  );
            vertex.time = source && source[v]
                ? source[v].time
                : Math.randomInRange(0, Math.PIM2);
            vertex.gradient = source && source[v]
                ? source[v].gradient
                : Math.randomInRange(0.2, 0.7);
        }

        light.setPosition(150, -50, 100);
        highlight.setPosition(0, 150, 15);


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

  //  console.log(scene);
    return scene;
}

module.exports = buildFSS;
