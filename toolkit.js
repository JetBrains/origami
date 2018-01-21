var registerToolkit = function() {

Rpd.channeltype('jb/layers', {
    show: function(v) {
        return howMuch('layer', 'layers')(v.values);
    }
});

var MAX_LAYERS = 9;

var LAYERS_INLETS = {};
LAYERS_INLETS['background'] = { type: 'util/color', default: { r: 66, g: 66, b: 66 } };
for (var i = 0; i < MAX_LAYERS; i++) {
    LAYERS_INLETS['layer-' + (i + 1)] =  { type: 'jb/drawable' }
};

LAYERS_INLETS['renderOptions'] = { type: 'core/any', hidden: true };

var DEFAULT_LAYER_OPTIONS = {
    blendMode: '',
    opacity: 1
};

var lastLayersConfig;
Rpd.nodetype('jb/layers', {
    title: 'Layers',
    inlets: LAYERS_INLETS,
    outlets: {
        'layers': { type: 'jb/layers' }
    },
    process: function(inlets) {
        //if (!inlets.renderOptions) return;
        var renderOptions = inlets.renderOptions;
        var layers = [];
        var layer;
        for (var i = 0; i < MAX_LAYERS; i++) {
            layer = inlets['layer-' + (i + 1)];
            if (layer && layer != 'dark') {
                layers.push([ inlets['layer-' + (i + 1)],
                              renderOptions ? renderOptions[i] : DEFAULT_LAYER_OPTIONS ]);
            }
        }
        if (!layers.length) return;
        return {
            'layers': { background: inlets.background,
                        values: layers }
        }
    }
});

};

module.exports = function() {
    registerToolkit();
};
