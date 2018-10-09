const dat = require('dat.gui');

const Config = function() {
    this.speed = 540;
    this.facesX = 12;
    this.facesY = 15;
    //this.explode = function() { ... };
};


function start(layers, updateLayers) {
    function adaptExtConfig(f) {
        return function(value) {
            if (typeof updateLayers !== 'undefined') {
                updateLayers(function(prevConfig) {
                    return f(value, prevConfig);
                });
            }
        }
    }


    console.log(dat);
    const config = new Config();
    const gui = new dat.GUI();
    const speed = gui.add(config, 'speed').min(100).max(1140);
    const facesX = gui.add(config, 'facesX').min(0).max(140);
    const facesY = gui.add(config, 'facesY').min(0).max(140);

    speed.onChange(adaptExtConfig(function(value, prevConfig) {
        prevConfig.lights.speed = value;
        return prevConfig;
    }));
    facesX.onChange(adaptExtConfig(function(value, prevConfig) {
        prevConfig.faces = [ value, prevConfig.faces[1] ];
        return prevConfig;
    }));
    facesY.onChange(adaptExtConfig(function(value, prevConfig) {
        prevConfig.faces = [ prevConfig.faces[0], value ];
        return prevConfig;
    }));
}

module.exports = start;
