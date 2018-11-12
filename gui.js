const deepClone = require('./deep-clone.js');
const dat = require('dat.gui');


const PRODUCTS = [
    { label: 'JB', id: 'jetbrains'
    },  // jetbrains-1
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
    },  // mps // MPS_
    { label: 'Mono', id: 'mono'
  }  // mono
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


RENDER_MODES =
  [ 'triangles', 'lines', 'partial-lines', 'points' ]


PREDEFINED_SIZES =
  { 'window': [0, 0]
  , '1920x1980': [ 1920, 1980 ]
  , '1366x768': [ 1366, 768 ]
  , '1440x900': [ 1440, 900 ]
  , '1536x864': [ 1536, 864 ]
  , '1680x1050': [ 1680, 1050 ]
  , '800 x 418': [ 800, 418 ]
  , '640 x 400': [ 640, 400 ]
  , '1280 x 800': [ 1280, 800 ]
  , '480x360': [ 480, 360 ]
  };

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const Config = function(layers, defaults, funcs) {
    const customAdd = BLEND_FUNCS['+'];
    const one = BLEND_FACTORS['1'];
    const zero = BLEND_FACTORS['0'];

    this.product = defaults.product;

    const funcKeys = Object.keys(BLEND_FUNCS);
    const factorKeys = Object.keys(BLEND_FACTORS);
    layers.forEach((layer, index) => {
      if (layer.webglOrSvg == 'webgl') {
        if (layer.blend[0]) {
          const blend = layer.blend[0];
          this['blendColor' + index] = blend.color || [ 1, 0, 0, 0 ]; // FIXME: get RGBA components
          this['blendColorEqFn' + index] = BLEND_FUNCS[funcKeys[blend.colorEq[0]]];
          this['blendColorEqFactor0' + index] = BLEND_FACTORS[factorKeys[blend.colorEq[1]]];
          this['blendColorEqFactor1' + index] = BLEND_FACTORS[factorKeys[blend.colorEq[2]]];
          this['blendAlphaEqFn' + index] = BLEND_FUNCS[funcKeys[blend.alphaEq[0]]];
          this['blendAlphaEqFactor0' + index] = BLEND_FACTORS[factorKeys[blend.alphaEq[1]]];
          this['blendAlphaEqFactor1' + index] = BLEND_FACTORS[factorKeys[blend.alphaEq[2]]];
        } else {
          this['blendColor' + index] = [ 0, 0, 0, 0 ];
          this['blendColorEqFn' + index] = customAdd;
          this['blendColorEqFactor0' + index] = one;
          this['blendColorEqFactor1' + index] = zero;
          this['blendAlphaEqFn' + index] = customAdd;
          this['blendAlphaEqFactor0' + index] = one;
          this['blendAlphaEqFactor1' + index] = zero;
        }
      } else {
        this['layer' + index + 'Blend'] = layer.blend[1] || 'normal';
      }
      this['visible' + index] = true;
      this['renderMode' + index] = 'triangles';

      if (isFss(layer)) {
        this['lightSpeed' + index] = defaults.fss.lightSpeed;
        this['facesX' + index] = defaults.fss.faces[0];
        this['facesY' + index] = defaults.fss.faces[1];
        this['vignette' + index] = defaults.fss.vignette;
        this['iris' + index] = defaults.fss.iris;
        this['amplitudeX' + index] = defaults.fss.amplitude[0];
        this['amplitudeY' + index] = defaults.fss.amplitude[1];
        this['amplitudeZ' + index] = defaults.fss.amplitude[2];
      }
    });

    this.customSize = defaults.customSize || PREDEFINED_SIZES['window'];

    this.savePng = funcs.savePng;
    // -------
    //this.timeShift = 0;
    // this.getSceneJson = funcs.getSceneJson;
    // this.loadSceneJson = funcs.loadSceneJson;
    //this.exportZip = funcs.exportZip;
};


function start(document, layers, defaults, funcs) {
    function updateProduct(id) {
      const product = PRODUCTS_BY_ID[id];
      funcs.changeProduct(product.id);
    }

    function switchLayer(index, on) {
      if (on) funcs.turnOn(index);
      else funcs.turnOff(index);
    }

    function updateWebGLBlend(index, f) {
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
        funcs.changeWGLBlend(index, newBlend);
      }
    }

    function addWebGLBlend(folder, config, layer, index) {
      const blendFolder = folder.addFolder('blend');
      const color = blendFolder.addColor(config, 'blendColor' + index);
      const colorEqFn = blendFolder.add(config, 'blendColorEqFn' + index, BLEND_FUNCS);
      const colorEqFactor0 = blendFolder.add(config, 'blendColorEqFactor0' + index, BLEND_FACTORS);
      const colorEqFactor1 = blendFolder.add(config, 'blendColorEqFactor1' + index, BLEND_FACTORS);
      const alphaEqFn = blendFolder.add(config, 'blendAlphaEqFn' + index, BLEND_FUNCS);
      const alphaEqFactor0 = blendFolder.add(config, 'blendAlphaEqFactor0' + index, BLEND_FACTORS);
      const alphaEqFactor1 = blendFolder.add(config, 'blendAlphaEqFactor1' + index, BLEND_FACTORS);
      //folder.open();

      color.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.color = { r: value[0], g: value[1], b: value[2], a: value[3] }
        return blend;
      }));
      colorEqFn.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.colorEq[0] = BLEND_FUNCS_IDS[value];
        return blend;
      }));
      colorEqFactor0.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.colorEq[1] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
      colorEqFactor1.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.colorEq[2] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
      alphaEqFn.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.alphaEq[0] = BLEND_FUNCS_IDS[value];
        return blend;
      }));
      alphaEqFactor0.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.alphaEq[1] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
      alphaEqFactor1.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        blend.alphaEq[2] = BLEND_FACTORS_IDS[value];
        return blend;
      }));
    }

    function addSVGBlend(folder, config, layer, index) {
      const blendControl = folder.add(config, 'layer' + index + 'Blend', SVG_BLENDS).name('blend');
      blendControl.onFinishChange((value) => {
        funcs.changeSVGBlend(index, value);
      });
    }

    function addLayerProps(folder, config, layer, index) {
      if (isFss(layer)) {
        const lightSpeed = folder.add(config, 'lightSpeed' + index).name('lights agile')
                                 .min(100).max(1140);
        const facesX = folder.add(config, 'facesX' + index).name('col').min(1).max(100).step(1);
        const facesY = folder.add(config, 'facesY' + index).name('row').min(1).max(100).step(1);
        const vignette = folder.add(config, 'vignette' + index).name('vignette').min(0.0).max(1.0);
        const iris = folder.add(config, 'iris' + index).name('iris').min(0.0).max(1.0);
        const renderMode = folder.add(config, 'renderMode' + index, RENDER_MODES).name('mesh');
        const amplitudeFolder = folder.addFolder('amplitude');
        const amplitudeX = amplitudeFolder.add(config, 'amplitudeX' + index).name('amplitudeX')
          .min(0.0).max(1.0);
        const amplitudeY = amplitudeFolder.add(config, 'amplitudeY' + index).name('amplitudeY')
          .min(0.0).max(1.0);
        const amplitudeZ = amplitudeFolder.add(config, 'amplitudeZ' + index).name('amplitudeZ')
          .min(0.0).max(1.0);

        lightSpeed.onFinishChange(funcs.changeLightSpeed(index));
        facesX.onFinishChange(funcs.changeFacesX(index));
        facesY.onFinishChange(funcs.changeFacesY(index));
        vignette.onFinishChange(funcs.changeVignette(index));
        iris.onFinishChange(funcs.changeIris(index));
        renderMode.onFinishChange(funcs.changeRenderMode(index));
        amplitudeX.onFinishChange(value => {
          funcs.changeAmplitude(index)(value, null, null);
        });
        amplitudeY.onFinishChange(value => {
          funcs.changeAmplitude(index)(null, value, null);
        });
        amplitudeZ.onFinishChange(value => {
          funcs.changeAmplitude(index)(null, null, value);
        });
      }
    }

    const config = new Config(layers, defaults, funcs);
    const gui = new dat.GUI(/*{ load: JSON }*/);
    gui.remember(config);
    const product = gui.add(config, 'product', PRODUCT_TO_ID);
    const customSize = gui.add(config, 'customSize', PREDEFINED_SIZES).name('size preset');
    const savePng = gui.add(config, 'savePng').name('save png');
    product.onFinishChange(funcs.changeProduct);
    customSize.onFinishChange(funcs.setCustomSize);

    layers.reverse().forEach((layer, revIndex) => {
      const index = layers.length - 1 - revIndex;
      //const folder = gui.addFolder('Layer ' + index + ' (' + layer.kind + ')');
      const folder = gui.addFolder(layer.name.toLowerCase());

      const visibitySwitch = folder.add(config, 'visible' + index).name('visible');
      visibitySwitch.onFinishChange(val => switchLayer(index, val));

      if (layer.webglOrSvg == 'webgl') {
        addWebGLBlend(folder, config, layer, index);
      } else {
        addSVGBlend(folder, config, layer, index);
      }
      addLayerProps(folder, config, layer, index);
    });

    let guiHidden = false;

    document.addEventListener('keydown', (event) => {
        if (event.keyCode == 32) {
            if (guiHidden) {
              document.querySelectorAll('.dg')[0].style.display = 'block';
              gui.open();
            } else { 
              document.querySelectorAll('.dg')[0].style.display = 'none';
              gui.close();
            }
            guiHidden = !guiHidden;
        }
      });


    // const textBlend = gui.add(config, 'textBlend', SVG_BLENDS);
    // textBlend.onFinishChange((value) => {
    //   funcs.changeSVGBlend(2, value);
    // });

    // const logoBlend = gui.add(config, 'logoBlend', SVG_BLENDS);
    // logoBlend.onFinishChange((value) => {
    //   funcs.changeSVGBlend(3, value);
    // });


    updateProduct('jetbrains');

    // layers.map((layer, index) => {
    //     gui.addFolder()
    // });
}

module.exports = start;
