const deepClone = require('./deep-clone.js');
const dat = require('dat.gui');

const PRODUCTS = [
    { label: 'JB', id: 'jetbrains'
    },  // jetbrains-1
    { label: 'D1', id: 'default1'
    },  // default-1
    { label: 'D2', id: 'default2'
    },  // default-2
    { label: ' ',  id: 'empty0'
    },  // separator
    { label: 'IJ', id: 'intellij-idea'
    },  // idea // IJ_
    { label: 'PS', id: 'phpstorm'
    },  // phpstorm // PS_
    { label: 'PC', id: 'pycharm'
    },  // pycharm // PC_
    { label: 'RM', id: 'rubymine'
    },  // rubymine // RM_
    { label: 'WS', id: 'webstorm'
    },  // webstorm // WS_
    { label: 'CL', id: 'clion'
    },  // clion // CL_
    { label: 'DG', id: 'datagrip'
    },  // // DG_
    { label: 'AC', id: 'appcode'
    },  // appcode // AC_
    { label: 'GO', id: 'gogland'
    },  // gogland // GO_
    { label: ' ', id: 'empty1'
    },  // separator
    { label: 'R#', id: 'resharper'
    },  // resharper // R#_
    { label: 'R++', id: 'resharper-cpp'
    },  // // R++_
    { label: 'DC', id: 'dotcover'
    },  // dotcover // DC_
    { label: 'DM', id: 'dotmemory'
    },  // // DM_
    { label: 'DP', id: 'dotpeek'
    },  // // DP_
    { label: 'DT', id: 'dottrace'
    },  // dottrace // DT_
    { label: 'RD', id: 'rider'
    },  // rider // RD_
    { label: ' ', id: 'empty2',
      palette: [ ]
    },  // separator
    { label: 'TC', id: 'teamcity'
    },  // teamcity // TC_
    { label: 'YT', id: 'youtrack'
    },  // youtrack // YT_
    { label: 'UP', id: 'upsource'
    },  // upsource // UP_
    { label: 'HB', id: 'hub'
    },  // hub // HB_
    { label: ' ', id: 'empty3'
    },  // separator
    { label: 'KT', id: 'kotlin'
    },   // kotlin // KT_
    { label: 'MPS', id: 'mps'
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

SVG_BLENDS =
  [ 'normal', 'overlay' ];

const Config = function(defaults, funcs) {
    const customAdd = BLEND_FUNCS['+'];
    const one = BLEND_FACTORS['1'];
    const zero = BLEND_FACTORS['0'];

    this.lightSpeed = defaults.lightSpeed;
    this.facesX = defaults.facesX;
    this.facesY = defaults.facesY;
    this.product = defaults.product;
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
    this.logoBlend = 'normal';
    // -------
    //this.timeShift = 0;
    // this.getSceneJson = funcs.getSceneJson;
    // this.loadSceneJson = funcs.loadSceneJson;
    //this.exportZip = funcs.exportZip;
};


function start(layers, defaults, funcs) {
    function updateProduct(id) {
      const product = PRODUCTS_BY_ID[id];
      funcs.changeProduct(product.id);
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
        funcs.changeWGLBlend(index, newBlend);
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


    const config = new Config(defaults, funcs);
    const gui = new dat.GUI(/*{ load: JSON }*/);
    gui.remember(config);
    const lightSpeed = gui.add(config, 'lightSpeed').min(100).max(1140);
    const facesX = gui.add(config, 'facesX').min(0).max(140).step(1);
    const facesY = gui.add(config, 'facesY').min(0).max(140).step(1);
    const product = gui.add(config, 'product', PRODUCT_TO_ID);

    lightSpeed.onFinishChange(funcs.changeLightSpeed);
    facesX.onFinishChange(funcs.changeFacesX);
    facesY.onFinishChange(funcs.changeFacesY);
    product.onFinishChange(funcs.changeProduct);

    addBlend(gui, config, 0);
    addBlend(gui, config, 1);

    const textBlend = gui.add(config, 'textBlend', SVG_BLENDS);
    textBlend.onFinishChange((value) => {
      funcs.changeSVGBlend(2, value);
    });

    const logoBlend = gui.add(config, 'logoBlend', SVG_BLENDS);
    logoBlend.onFinishChange((value) => {
      funcs.changeSVGBlend(3, value);
    });

    updateProduct('jetbrains');

    // layers.map((layer, index) => {
    //     gui.addFolder()
    // });
}

module.exports = start;
