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


const RELEASE_SIZES = // TODO: Multiply for creating @2x @3x
    { '480x297 prodcard' : [ 480, 297 ] //product card
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
    , 'browser': [ 0, 0 ]
};



const ADS_SIZES = // TODO: Multiply for creating @2x @3x
    { '120x600' : [ 120, 600 ]
    , '125x125' : [ 125, 125 ]
    , '130x100' : [ 130, 100 ]
    , '180x150' : [ 180, 150 ]
    , '200x125' : [ 200, 125 ]
    , '200x200' : [ 200, 200 ]
    , '220x250' : [ 220, 250 ]
    , '250x250' : [ 250, 250 ]
    , '260x200' : [ 260, 200 ]
    , '300x250' : [ 300, 250 ]
    , '320x100' : [ 320, 100 ]
    , '320x50' : [ 320, 50 ]
    , '336x280' : [ 336, 280 ]
    , '468x60' : [ 468, 60 ]
    , '160x600' : [ 160, 600 ]
    , '300x600' : [ 300, 600 ]
    , '728x90' : [ 728, 90 ]
    , '800x320' : [ 800, 320 ]
    , '970x250' : [ 970, 250 ]
    , '970x90' : [ 970, 90 ]
    , '960x90 baidu' : [ 960, 90 ]
    , '728x90 baidu' : [ 728, 90 ]
    , '468x60 baidu' : [ 468, 60 ]
    , '200x200 baidu' : [ 200, 200 ]
    , '960x60 baidu' : [ 960, 60 ]
    , '640x60 baidu' : [ 640, 60 ]
    , '580x90 baidu' : [ 580, 90 ]
    , '460x60 baidu' : [ 460, 60 ]
    , '300x250 baidu' : [ 300, 250 ]
    , '336x280 baidu' : [ 336, 280 ]
    , '1200x628 fb' : [ 1200, 628 ]
    , '800x418 tw' : [ 800, 418 ]
    , '1080x1080 in' : [ 1080, 1080 ]
    , '1200x627 ln' : [ 1200, 627 ]
    , 'browser': [ 0, 0 ]
};


const WALLPAPER_SIZES =
    { '2560x1440': [ 2560, 1440 ]
    , '1920x1200': [ 1920, 1200 ]
    , '1920x1080': [ 1920, 1080 ]
    , '1680x1050': [ 1680, 1050 ]
    , '1536x864': [ 1536, 864 ]
    , '1440x900': [ 1440, 900 ]
    , '1366x768': [ 1366, 768 ]
    , 'browser': [ 0, 0 ]
};


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
    RELEASE_SIZES,
    ADS_SIZES,
    WALLPAPER_SIZES,
    funcKeys,
    factorKeys,
    setsKeys
}
