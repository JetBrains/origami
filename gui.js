const deepClone = require('./deep-clone.js');
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
});
const PRODUCTS_BY_ID = {};
PRODUCTS.forEach((product) => {
    PRODUCTS_BY_ID[product.id] = product;
});

BLEND_FUNCS =
  { '+': 'customAdd'
  , '-': 'customSubtract'
  , 'R-': 'reverseSubtract'
  };

BLEND_FUNCS_IDS =
  { 'customAdd': 0
  , 'customSubtract': 1
  , 'reverseSubtract': 2
  };

BLEND_FACTORS =
  { '0': 'zero'
  , '1': 'one'
  , 'sC': 'srcColor'
  , '1-sC': 'oneMinusSrcColor'
  , 'dC': 'dstColor'
  , '1-dC': 'oneMinusDstColor'
  , 'sA': 'srcAlpha'
  , '1-sA': 'oneMinusSrcAlpha'
  , 'dA': 'dstAlpha'
  , '1-dA': 'oneMinusDstAlpha'
  , 'AS': 'srcAlphaSaturate'
  , 'CC': 'constantColor'
  , '1-CC': 'oneMinusConstantColor'
  , 'CA': 'constantAlpha'
  , '1-CA': 'oneMinusConstantAlpha'
};

BLEND_FACTORS_IDS =
  { 'zero': 0
  , 'one': 1
  , 'srcColor': 2
  , 'oneMinusSrcColor': 3
  , 'dstColor': 4
  , 'oneMinusDstColor': 5
  , 'srcAlpha': 6
  , 'oneMinusSrcAlpha': 7
  , 'dstAlpha': 8
  , 'oneMinusDstAlpha': 9
  , 'srcAlphaSaturate': 10
  , 'constantColor': 11
  , 'oneMinusConstantColor': 12
  , 'constantAlpha': 13
  , 'oneMinusConstantAlpha': 14
};

TEXT_BLENDS =
  [ 'normal', 'overlay' ];

const Config = function() {
    const customAdd = BLEND_FUNCS['+'];
    const one = BLEND_FACTORS['1'];
    const zero = BLEND_FACTORS['0'];

    this.lightSpeed = 540;
    this.facesX = 12;
    this.facesY = 15;
    this.palette = 'jetbrains';
    this.blendColor0 = [ 0, 0, 0, 0 ];
    this.blendColorEqFn0 = customAdd;
    this.blendColorEqFactor00 = one;
    this.blendColorEqFactor10 = zero;
    this.blendAlphaEqFn0 = customAdd;
    this.blendAlphaEqFactor00 = one;
    this.blendAlphaEqFactor10 = zero;
    this.blendColor1 = [ 0, 0, 0, 0 ];
    this.blendColorEqFn1 = customAdd;
    this.blendColorEqFactor01 = one;
    this.blendColorEqFactor11 = zero;
    this.blendAlphaEqFn1 = customAdd;
    this.blendAlphaEqFactor01 = one;
    this.blendAlphaEqFactor11 = zero;
    this.textBlend = 'normal';
    //this.explode = function() { ... };
};


function start(layers, updateLayers, updateColors, changeWGLBlend, changeSVGBlend) {
    function adaptExtConfig(f) {
        return function(value) {
            updateLayers((prevConfig) => {
                return f(value, prevConfig);
            });
        }
    }

    function updateBlend(index, f) {
      return function(value) {
        const color = config['blendColor'+index];
        const curBlend =
          { color: { r: color[0], g: color[1], b: color[2], a: color[3] }
          , colorEq: [ BLEND_FUNCS_IDS[config['blendColorEqFn'+index]]
                     , BLEND_FACTORS_IDS[config['blendColorEqFactor0'+index]]
                     , BLEND_FACTORS_IDS[config['blendColorEqFactor1'+index]]
                     ]
          , alphaEq: [ BLEND_FUNCS_IDS[config['blendAlphaEqFn'+index]]
                     , BLEND_FACTORS_IDS[config['blendAlphaEqFactor0'+index]]
                     , BLEND_FACTORS_IDS[config['blendAlphaEqFactor1'+index]]
                     ]
          }
        const newBlend = f(curBlend, value);
        console.log('new blend', index, newBlend)
        console.log('new blend', index, 'color', config['blendColor'+index])
        console.log('new blend', index, 'colorEq',
          config['blendColorEqFn'+index],
          config['blendColorEqFactor0'+index],
          config['blendColorEqFactor1'+index]);
        console.log('new blend', index, 'alphaEq',
          config['blendAlphaEqFn'+index],
          config['blendAlphaEqFactor0'+index],
          config['blendAlphaEqFactor1'+index]);
        changeWGLBlend(index, newBlend);
      }
    }

    function addBlend(gui, config, index) {

      const folder = gui.addFolder('Blend' + index);
      const color = folder.addColor(config, 'blendColor' + index);
      const colorEqFn = folder.add(config, 'blendColorEqFn' + index, BLEND_FUNCS);
      const colorEqFactor0 = folder.add(config, 'blendColorEqFactor0' + index, BLEND_FACTORS);
      const colorEqFactor1 = folder.add(config, 'blendColorEqFactor1' + index, BLEND_FACTORS);
      const alphaEqFn = folder.add(config, 'blendAlphaEqFn' + index, BLEND_FUNCS);
      const alphaEqFactor0 = folder.add(config, 'blendAlphaEqFactor0' + index, BLEND_FACTORS);
      const alphaEqFactor1 = folder.add(config, 'blendAlphaEqFactor1' + index, BLEND_FACTORS);
      //folder.open();

      color.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('color', index, value);
        blend.color = { r: value[0], g: value[1], b: value[2], a: value[3] }
        return blend;
      }));
      colorEqFn.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('colorEqFn', index, value);
        blend.colorEq[0] = BLEND_FUNCS_IDS[value];
        return blend;
      }));
      colorEqFactor0.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('colorEqFactor0', index, value);
        blend.colorEq[1] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
      colorEqFactor1.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('colorEqFactor1', index, value);
        blend.colorEq[2] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
      alphaEqFn.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('alphaEqFn', index, value);
        blend.alphaEq[0] = BLEND_FUNCS_IDS[value];
        return blend;
      }));
      alphaEqFactor0.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('alphaEqFactor0', index, value);
        blend.alphaEq[1] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
      alphaEqFactor1.onFinishChange(updateBlend(index, (blend, value) => {
        console.log('alphaEqFactor1', index, value);
        blend.alphaEq[2] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
    }


    const config = new Config();
    const gui = new dat.GUI(/*{ load: JSON }*/);
    gui.remember(config);
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

    addBlend(gui, config, 0);
    addBlend(gui, config, 1);

    const textBlend = gui.add(config, 'textBlend', TEXT_BLENDS);
    textBlend.onFinishChange((value) => {
      changeSVGBlend(2, value);
    });

    // layers.map((layer, index) => {
    //     gui.addFolder()
    // });
}

module.exports = start;
