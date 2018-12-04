const deepClone = require('./deep-clone.js');
const {
    PRODUCTS,
    RENDER_MODES,
    BLEND_FUNCS,
    BLEND_FACTORS,
    BLEND_SETS,
    BLEND_FUNCS_IDS,
    BLEND_FACTORS_IDS,
    funcKeys,
    factorKeys,
    setsKeys
} = require('./constants.js');
const isFss = require('./is-fss.js');

const randomize = (applyRandomizer, model, updateGui) => (config) => () => {
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

          const opacity =  (mode !== 'prod') ? Math.random() : 1.0;
          layerDef.model.opacity = opacity;
          config['opacity' + index] = opacity;

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

            const blendSetIndex = [ Math.floor(Math.random() * Object.values(BLEND_SETS).length) ];
            const blendSet = BLEND_SETS[setsKeys[blendSetIndex]];
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
    if (updateGui) updateGui();
    // });
    applyRandomizer(toSend);
  }


module.exports = randomize;
