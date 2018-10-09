const dat = require('dat.gui');

const PRODUCTS = [
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
const PRODUCT_TO_ID = {};
PRODUCTS.forEach((product) => {
    PRODUCT_TO_ID[product.label] = product.id;
})
const PRODUCTS_BY_ID = {};
PRODUCTS.forEach((product) => {
    PRODUCTS_BY_ID[product.id] = product;
})

const Config = function() {
    this.lightSpeed = 540;
    this.facesX = 12;
    this.facesY = 15;
    this.palette = 'jetbrains';
    this.color0 = '';
    this.color1 = '';
    this.color2 = '';
    //this.explode = function() { ... };
};


function start(layers, updateLayers, updateColors) {
    function adaptExtConfig(f) {
        return function(value) {
            updateLayers(function(prevConfig) {
                return f(value, prevConfig);
            });
        }
    }


    console.log(dat);
    const config = new Config();
    const gui = new dat.GUI();
    const lightSpeed = gui.add(config, 'lightSpeed').min(100).max(1140);
    const facesX = gui.add(config, 'facesX').min(0).max(140).step(1);
    const facesY = gui.add(config, 'facesY').min(0).max(140).step(1);
    const palette = gui.add(config, 'palette', PRODUCT_TO_ID);

    lightSpeed.onFinishChange(adaptExtConfig(function(value, prevConfig) {
        prevConfig.lights.speed = value;
        return prevConfig;
    }));
    facesX.onFinishChange(adaptExtConfig(function(value, prevConfig) {
        prevConfig.faces = [ value, prevConfig.faces[1] ];
        return prevConfig;
    }));
    facesY.onFinishChange(adaptExtConfig(function(value, prevConfig) {
        prevConfig.faces = [ prevConfig.faces[0], value ];
        return prevConfig;
    }));
    palette.onFinishChange((value) => {
        const palette = PRODUCTS_BY_ID[value].palette;
        updateColors(0, palette.slice(0, 2));
        updateColors(1, palette.slice(1, 2));
    });

    // layers.map((layer, index) => {
    //     gui.addFolder()
    // });
}

module.exports = start;
