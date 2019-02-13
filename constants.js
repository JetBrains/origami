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


const HTML_BLENDS =
  [ 'normal', 'overlay' ];


const RENDER_MODES =
  [ 'triangles', 'lines' ] // disable ", 'partial-lines', 'points' modes"


// RELEASE_SIZES

// ADS_SIZES

// WALLPAPER_SIZES


const BLEND_SETS =
  { 'normal': [ [ '+', '1', '0' ], [ '+', '1', '0' ] ]
  , 'soft': [ [ '+', 'sC', '1-sC' ], [ '+', 'sC', '1-sC' ] ]
  , 'caustic': [ [ '+', 'sC', '1' ], [ '+', '1', '0' ] ]
  , 'kobold': [ [ '+', 'sC', '0' ], [ '+', '1', '0' ] ]
  , 'crystal': [ [ '+', 'sC', 'sC' ], [ '+', '1', '0' ] ]
  , 'dark swan': [ [ '+', '0', 'sC' ], [ '+', '1', '0' ] ]
  , 'chromatic': [ [ '+', '1', '1-CC' ], [ '+', '1', '0' ] ]
  , 'opalescent': [ [ '+', 'sC', '1-CC' ], [ '+', '1', '0' ] ]
  , 'plastic': [ [ '+', 'sC', 'AS' ], [ '+', '1', '0' ] ]
  , 'oz': [ [ '-', '1', '1' ], [ '+', '1', '0' ] ]
  , 'smokey': [ [ '-', '1', 'sC' ], [ '+', '1', '0' ] ]
  , 'shining': [ [ '-', '1', 'CC' ], [ '+', '1', '0' ] ]
  , 'mist': [ [ '-', '1-sC', '1-dA' ], [ '+', '1', '0' ] ]
  , 'elmo': [ [ 'R-', 'sC', 'sA' ], [ '+', '1', '0' ] ]
  };


const funcKeys = Object.keys(BLEND_FUNCS);
const factorKeys = Object.keys(BLEND_FACTORS);
const setsKeys = Object.keys(BLEND_SETS);

module.exports = {
    PRODUCTS,
    PRODUCT_TO_ID,
    PRODUCTS_BY_ID,
    BLEND_FUNCS,
    BLEND_FUNCS_IDS,
    BLEND_FACTORS,
    BLEND_FACTORS_IDS,
    BLEND_SETS,
    HTML_BLENDS,
    RENDER_MODES,
    //RELEASE_SIZES,
    //ADS_SIZES,
    //WALLPAPER_SIZES,
    funcKeys,
    factorKeys,
    setsKeys
}
