const deepClone = require('./deep-clone.js');
const dat = require('dat.gui');


const PRODUCTS = [
    { label: 'JetBrains', id: 'jetbrains'
    },  // jetbrains-1
    { label: 'IntelliJ IDEA', id: 'intellij-idea'
    },  // idea // IJ_
    { label: 'PhpStorm', id: 'phpstorm'
    },  // phpstorm // PS_
    { label: 'PyCharm', id: 'pycharm'
    },  // pycharm // PC_
    { label: 'RubyMine', id: 'rubymine'
    },  // rubymine // RM_
    { label: 'WebStorm', id: 'webstorm'
    },  // webstorm // WS_
    { label: 'CLion', id: 'clion'
    },  // clion // CL_
    { label: 'DataGrip', id: 'datagrip'
    },  // // DG_
    { label: 'AppCode', id: 'appcode'
    },  // appcode // AC_
    { label: 'GoLand', id: 'goland'
    },  // goland // GO_
    { label: 'ReSharper', id: 'resharper'
    },  // resharper // R#_
    { label: 'ReSharper C++', id: 'resharper-cpp'
    },  // // R++_
    { label: 'dotCover', id: 'dotcover'
    },  // dotcover // DC_
    { label: 'dotMemory', id: 'dotmemory'
    },  // // DM_
    { label: 'dotPeek', id: 'dotpeek'
    },  // // DP_
    { label: 'dotTrace', id: 'dottrace'
    },  // dottrace // DT_
    { label: 'Rider', id: 'rider'
    },  // rider // RD_
    { label: 'TeamCity', id: 'teamcity'
    },  // teamcity // TC_
    { label: 'YouTrack', id: 'youtrack'
    },  // youtrack // YT_
    { label: 'Upsource', id: 'upsource'
    },  // upsource // UP_
    { label: 'Hub', id: 'hub'
    },  // hub // HB_
    { label: 'Kotlin', id: 'kotlin'
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


const BLEND_FUNCS =
  { '+': 'customAdd'
  , '-': 'customSubtract'
  , 'R-': 'reverseSubtract'
  };


const BLEND_FUNCS_IDS =
  { 'customAdd': 0
  , 'customSubtract': 1
  , 'reverseSubtract': 2
  };


const BLEND_FACTORS =
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


const BLEND_FACTORS_IDS =
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


const SVG_BLENDS =
  [ 'normal', 'overlay' ];


const RENDER_MODES =
  [ 'triangles', 'lines' ] // disable ", 'partial-lines', 'points' modes"


const RELEASE_SIZES = // TODO: Multiply for creating @2x @3x
{ 'window': [ 0, 0 ]
, '480x297 prodcard' : [ 480, 297 ] //product card
, '960x594 prodcard@2x' : [ 960, 594 ] //@2x product card
, '640x400 spl' : [ 640, 400 ] // product splash background
, '1280x800 spl@2x' : [ 1280, 800 ] // @2x splash background
, '650x170 nwlt' : [ 650, 170 ] // newsletter
, '1300x340 nwlt@2x' : [ 1300, 340 ] // @2x newsletter
, '800x418 tw' : [ 800, 418 ] // Twitter
, '1200x628 fb' : [ 1200, 628 ] // Facebook
, '1280x800 wprev' : [ 1280, 800 ] // Webpage Preview
, '800x400 blog' : [ 800, 400 ] // Blog
, '1600x800 blog@2x' : [ 1600, 800 ] // @2x Blog
, '800x155 bfoot' : [ 800, 155 ] // Blog footer
, '1600x310 bfoot' : [ 1600, 310 ] // @2x Blog footer
, '2850x1200 landg' : [ 2850, 1200 ] // Landing page
};


const WALLPAPER_SIZES =
  { 'window': [0, 0]
  , '1920x1980': [ 1920, 1080 ]
  , '1920x1200': [ 1920, 1200 ]
  , '1366x768': [ 1366, 768 ]
  , '1440x900': [ 1440, 900 ]
  , '1536x864': [ 1536, 864 ]
  , '1680x1050': [ 1680, 1050 ]
  };


const BLENDS_SETS =
  { 'normal': [ [ '+', '1', '0' ], [ '+', '1', '0' ] ]
  , 'lighten': [ [ '+', 'sC', '1-sC' ], [ '+', 'sC', '1-sC' ] ]
  };


const funcKeys = Object.keys(BLEND_FUNCS);
const factorKeys = Object.keys(BLEND_FACTORS);
const setsKeys = Object.keys(BLENDS_SETS);

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';

const update = (gui) => () => {
  for (var i in gui.__controllers) {
    gui.__controllers[i].updateDisplay();
  }
  for (var i in gui.__folders) {
    update(gui.__folders[i])();
  }
}

const getSizesSet = (mode) =>
  (mode != 'prod') ? RELEASE_SIZES : WALLPAPER_SIZES;

const Config = function(layers, defaults, funcs, randomize) {
    const customAdd = BLEND_FUNCS['+'];
    const one = BLEND_FACTORS['1'];
    const zero = BLEND_FACTORS['0'];

    const mode = defaults.mode;
    this.product = defaults.product;
    this.omega = defaults.omega;

    const PREDEFINED_SIZES = getSizesSet(mode);

    layers.forEach((layer, index) => {
      if (layer.webglOrSvg == 'webgl') {
        if (mode !== 'prod') {

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

        } else { // mode == 'prod'
          this['blendSet' + index] = BLENDS_SETS['normal'];
          this['blendColor' + index] =
              layer.blend[0].color || [ 1, 0, 0, 0 ]; // FIXME: get RGBA components
        }
      } else { // webglOrSvg != 'webgl'
        this['layer' + index + 'Blend'] = layer.blend[1] || 'normal';
      }

      this['visible' + index] = true;

      if (isFss(layer)) {
        this['mirror' + index] = true;
        this['renderMode' + index] = 'triangles';
        this['lightSpeed' + index] = layer.model.lightSpeed;
        this['facesX' + index] = layer.model.faces[0];
        this['facesY' + index] = layer.model.faces[1];
        this['vignette' + index] = layer.model.vignette;
        this['iris' + index] = layer.model.iris;
        this['amplitudeX' + index] = layer.model.amplitude[0];
        this['amplitudeY' + index] = layer.model.amplitude[1];
        this['amplitudeZ' + index] = layer.model.amplitude[2];
        this['hue' + index] = layer.model.colorShift[0];
        this['saturation' + index] = layer.model.colorShift[1];
        this['brightness' + index] = layer.model.colorShift[2];
      }
    });

    this.customSize = PREDEFINED_SIZES['window'];

    //this.savePng = funcs.savePng;
    this.saveBatch = () => funcs.saveBatch(Object.values(PREDEFINED_SIZES));
    this.randomize = randomize(this);
    // -------
    //this.timeShift = 0;
    // this.getSceneJson = funcs.getSceneJson;
    // this.loadSceneJson = funcs.loadSceneJson;
    //this.exportZip = funcs.exportZip;
};

function start(document, model, funcs) {
    const defaults = model;
    const { mode, layers } = model;

    const PREDEFINED_SIZES = getSizesSet(mode);

    function updateProduct(id) {
      const product = PRODUCTS_BY_ID[id];
      funcs.changeProduct(product.id);
    }

    function switchLayer(index, on) {
      if (on) funcs.turnOn(index);
      else funcs.turnOff(index);
    }

    function switchMirror(index, on) {
      if (on) funcs.mirrorOn(index);
      else funcs.mirrorOff(index);
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
      if (mode !== 'prod') {

        const blendFolder = folder.addFolder('blend');
        const color = blendFolder.addColor(config, 'blendColor' + index);
        const colorEqFn = blendFolder.add(config, 'blendColorEqFn' + index, BLEND_FUNCS);
        const colorEqFactor0 =
            blendFolder.add(config, 'blendColorEqFactor0' + index, BLEND_FACTORS);
        const colorEqFactor1 =
            blendFolder.add(config, 'blendColorEqFactor1' + index, BLEND_FACTORS);
        const alphaEqFn =
            blendFolder.add(config, 'blendAlphaEqFn' + index, BLEND_FUNCS);
        const alphaEqFactor0 =
            blendFolder.add(config, 'blendAlphaEqFactor0' + index, BLEND_FACTORS);
        const alphaEqFactor1 =
            blendFolder.add(config, 'blendAlphaEqFactor1' + index, BLEND_FACTORS);
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

      } else { // mode == 'prod'

        const blendSet = folder.add(config, 'blendSet' + index, BLENDS_SETS).name('blend');
        // const blendColor = folder.addColor(config, 'blendColor' + index).name('color');

        // blendColor.onFinishChange(updateWebGLBlend(index, (blend, value) => {
        //   blend.color = { r: value[0], g: value[1], b: value[2], a: value[3] }
        //   return blend;
        // }));

        blendSet.onFinishChange((value) => {
          blendConfig = value.split(',');

          const color = config['blendColor'+index] || [ 0, 0, 0, 1 ];
          const newBlend =
          { color: { r: color[0], g: color[1], b: color[2], a: color[3] }
          , colorEq: [ BLEND_FUNCS_IDS[BLEND_FUNCS[blendConfig[0]]]
                     , BLEND_FACTORS_IDS[BLEND_FACTORS[blendConfig[1]]]
                     , BLEND_FACTORS_IDS[BLEND_FACTORS[blendConfig[2]]]
                     ]
          , alphaEq: [ BLEND_FUNCS_IDS[BLEND_FUNCS[blendConfig[3]]]
                     , BLEND_FACTORS_IDS[BLEND_FACTORS[blendConfig[4]]]
                     , BLEND_FACTORS_IDS[BLEND_FACTORS[blendConfig[5]]]
                     ]
          }
          funcs.changeWGLBlend(index, newBlend);
          // blend.alphaEq[2] = BLEND_FACTORS_IDS[value];
          //return blend;
        });

      }
    }

    function addSVGBlend(folder, config, layer, index) {
      const blendControl = folder.add(config, 'layer' + index + 'Blend', SVG_BLENDS).name('blend');
      blendControl.onFinishChange((value) => {
        funcs.changeSVGBlend(index, value);
      });
    }

    function addLayerProps(folder, config, layer, index) {
      if (isFss(layer)) {
        const mirrorSwitch = folder.add(config, 'mirror' + index).name('rorschach');
        const lightSpeed = folder.add(config, 'lightSpeed' + index).name('light pace')
                                 .min(100).max(2000);
        const facesX = folder.add(config, 'facesX' + index).name('columns').min(1).max(100).step(1);
        const facesY = folder.add(config, 'facesY' + index).name('rows').min(1).max(100).step(1);
        const fogFolder = folder.addFolder('fog');
        const vignette = fogFolder.add(config, 'vignette' + index).name('shine').min(0).max(1).step(0.01);
        const iris = fogFolder.add(config, 'iris' + index).name('density').min(0).max(1).step(0.01);
        const renderMode = folder.add(config, 'renderMode' + index, RENDER_MODES).name('structure');

        const amplitudeFolder = folder.addFolder('ranges');
        const amplitudeX = amplitudeFolder.add(config, 'amplitudeX' + index).name('horizontal')
          .min(0.0).max(1.0);
        const amplitudeY = amplitudeFolder.add(config, 'amplitudeY' + index).name('vertical')
          .min(0.0).max(1.0);
        const amplitudeZ = amplitudeFolder.add(config, 'amplitudeZ' + index).name('depth')
          .min(0.0).max(1.0);

        const colorShiftFolder = folder.addFolder('coloring');
        const hue = colorShiftFolder.add(config, 'hue' + index).name('color')
          .min(-1.0).max(1.0).step(0.01);
        const saturation = colorShiftFolder.add(config, 'saturation' + index).name('saturation')
          .min(-1.0).max(1.0).step(0.01);
        const brightness = colorShiftFolder.add(config, 'brightness' + index).name('brightness')
          .min(-1.0).max(1.0).step(0.01);

        mirrorSwitch.onFinishChange(val => switchMirror(index, val));
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

        hue.onFinishChange(value => {
          funcs.shiftColor(index)(value, null, null);
        });
        saturation.onFinishChange(value => {
          funcs.shiftColor(index)(null, value, null);
        });
        brightness.onFinishChange(value => {
          funcs.shiftColor(index)(null, null, value);
        });
      }
    }

    const gui = new dat.GUI(/*{ load: JSON }*/);
    const config = new Config(layers, defaults, funcs, randomize(funcs, model, update(gui)));
    const product = gui.add(config, 'product', PRODUCT_TO_ID);
    const omega = gui.add(config, 'omega').name('vertigo ').min(-1.0).max(1.0).step(0.1);
    const customSize = gui.add(config, 'customSize', PREDEFINED_SIZES).name('size preset');
    // gui.add(config, 'savePng').name('save png');
    if (mode !== 'prod') gui.add(config, 'saveBatch').name('save batch');
    gui.add(config, 'randomize').name('i feel lucky');
    product.onFinishChange(funcs.changeProduct);
    omega.onFinishChange(funcs.rotate);
    customSize.onFinishChange(funcs.setCustomSize);

    layers.concat([]).reverse().forEach((layer, revIndex) => {
      if ((mode == 'prod') && (layer.name == 'Cover')) return;

      const index = layers.length - 1 - revIndex;
      //const folder = gui.addFolder('Layer ' + index + ' (' + layer.kind + ')');
      const folder = gui.addFolder(layer.name.toLowerCase());

      const visibitySwitch = folder.add(config, 'visible' + index).name('visible');
      visibitySwitch.onFinishChange(val => switchLayer(index, val));

      addLayerProps(folder, config, layer, index);
      if (layer.webglOrSvg == 'webgl') {
        addWebGLBlend(folder, config, layer, index);
      } else {
        addSVGBlend(folder, config, layer, index);
      }
    });

    let guiHidden = false;

    //update(gui);

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


    //updateProduct('jetbrains');

    // layers.map((layer, index) => {
    //     gui.addFolder()
    // });
}

const randomize = (funcs, model, updateGui) => (config) => () => {
  const toSend = deepClone(model);
  const mode = model.mode;
  const omega = Math.random() * 2 - 1;
  const productIdx = Math.floor(Math.random() * PRODUCTS.length);
  const product = PRODUCTS[productIdx].id;

  config.product = product;
  config.omega = omega;

  toSend.product = product;
  toSend.omega = omega;

  toSend.layers.forEach((layerDef, index) => {
      // .visible
      if (isFss(layerDef)) {
        const mirror = Math.random() > 0.5 ? true : false;
        layerDef.model.mirror = mirror;
        config['mirror' + index] = mirror;

        const lightSpeed = Math.floor(Math.random() * 1040 + 100);
        layerDef.model.lightSpeed = lightSpeed;
        config['lightSpeed' + index] = lightSpeed;

        const renderModeIdx = Math.floor(Math.random() * RENDER_MODES.length);
        const renderMode = RENDER_MODES[renderModeIdx];
        layerDef.model.renderMode = renderMode;
        config['renderMode' + index] = renderMode;

        const facesMax = (mode !== 'prod') ? 100 : 50;
        const [ facesX, facesY ] =
          [ Math.floor(Math.random() * facesMax)
          , Math.floor(Math.random() * facesMax)
          ];
        layerDef.model.faces = [ facesX, facesY ];
        config['facesX' + index] = facesX;
        config['facesY' + index] = facesY;

        const iris = Math.random();
        layerDef.model.iris = iris;
        config['iris' + index] = iris;

        const vignette = Math.random();
        layerDef.model.vignette = vignette;
        config['vignette' + index] = vignette;

        const [ amplitudeX, amplitudeY, amplitudeZ ] =
          [ Math.random(), Math.random(), Math.random() ];
        layerDef.model.amplitude = [ amplitudeX, amplitudeY, amplitudeZ ];
        config['amplitudeX' + index] = amplitudeX;
        config['amplitudeY' + index] = amplitudeY;
        config['amplitudeZ' + index] = amplitudeZ;


        const [ hue, saturation, brightness ] =
          [ (mode !== 'prod') ? Math.random() * 2.0 - 1.0 : 0.0,
            Math.random() * 2.0 - 1.0,
            Math.random() * 2.0 - 1.0 ];
        layerDef.model.colorShift = [ hue, saturation, brightness ];
        config['hue' + index] = hue;
        config['saturation' + index] = saturation;
        config['brightness' + index] = brightness;
      }

      // comment this out to not randomize blends
      if (layerDef.webglOrSvg == 'webgl') {
        if (mode !== 'prod') {

          const blendFuncsCount = Object.values(BLEND_FUNCS_IDS).length;
          const blendFactorsCount = Object.values(BLEND_FACTORS_IDS).length;
          const colorEq =
            [ Math.floor(Math.random() * blendFuncsCount)
            , Math.floor(Math.random() * blendFactorsCount)
            , Math.floor(Math.random() * blendFactorsCount)
            ];
          const alphaEq =
            [ Math.floor(Math.random() * blendFuncsCount)
            , Math.floor(Math.random() * blendFactorsCount)
            , Math.floor(Math.random() * blendFactorsCount)
            ];

          //config['blendColor' + index] = [ 0, 0, 0, 0 ];
          config['blendColorEqFn' + index] = BLEND_FUNCS[funcKeys[colorEq[0]]];
          config['blendColorEqFactor0' + index] = BLEND_FACTORS[factorKeys[colorEq[0]]];
          config['blendColorEqFactor1' + index] = BLEND_FACTORS[factorKeys[colorEq[1]]];
          config['blendAlphaEqFn' + index] = BLEND_FUNCS[funcKeys[alphaEq[0]]];;
          config['blendAlphaEqFactor0' + index] = BLEND_FACTORS[factorKeys[alphaEq[0]]];
          config['blendAlphaEqFactor1' + index] = BLEND_FACTORS[factorKeys[alphaEq[1]]];
          // TODO: color { r: color[0], g: color[1], b: color[2], a: color[3] }
          layerDef.blend =
            [ { color: null
              , colorEq: colorEq
              , alphaEq: alphaEq
              }
            , null
            ];

        } else {

          const blendSetIndex = [ Math.floor(Math.random() * Object.values(BLENDS_SETS).length) ];
          const blendSet = BLENDS_SETS[setsKeys[blendSetIndex]];
          console.log(config['blendSet' + index], blendSet);
          config['blendSet' + index] = blendSet;
          // TODO:
          // layerDef.blend =
          //   [ { color: null
          //     , colorEq: []
          //     , alphaEq: alphaEq
          //     }
          //   , null
          //   ];

        }
      } else {

      }
  });
  // console.log(toSend);

  // toSend.layers = model.layers.reverse().map((layerDef, index) => {
  //     return layerDef;
  // });
  updateGui();
  // });
  funcs.applyRandomizer(toSend);
}

module.exports = start;
