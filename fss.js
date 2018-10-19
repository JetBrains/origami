window.FSSAvoidFloat32Array = true;

//if (typeof window.require !== 'undefined') {
require('flat-surface-shader/source/Core');
require('flat-surface-shader/source/Vector3');
require('flat-surface-shader/source/Vector4');
require('flat-surface-shader/source/Vertex');
require('flat-surface-shader/source/Color');
require('flat-surface-shader/source/Triangle');
require('flat-surface-shader/source/Object');
require('flat-surface-shader/source/Light');
require('flat-surface-shader/source/Material');
require('flat-surface-shader/source/Geometry');
require('flat-surface-shader/source/Plane');
require('flat-surface-shader/source/Mesh');
require('flat-surface-shader/source/Scene');
require('flat-surface-shader/source/Math');
//}

function buildFSS(model, source) {
    var palette = model.palette;

    var scene = new FSS.Scene();

    var geometry = new FSS.Plane(model.size[0], model.size[1],
                                 model.facesX, model.facesY);
    var material = new FSS.Material('#ffffff', '#ffffff');
    var mesh = new FSS.Mesh(geometry, material);

    var ambient = new FSS.Light(palette[0], palette[1]);
    var diffuse = new FSS.Light(palette[2], palette[2]);
    ambient.speed = model.lightSpeed;
    diffuse.speed = model.lightSpeed;

    function initialise() {
        scene.add(mesh);
        scene.add(ambient);
        scene.add(diffuse);

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

        ambient.setPosition(150, -50, 100);
        diffuse.setPosition(0, 150, 35);

        //container.appendChild(renderer.element);
        // window.addEventListener('resize', resize);
    }

    // function resize() {
    //     renderer.setSize(container.offsetWidth, container.offsetHeight);
    // }

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
