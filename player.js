require('./fss.js');
const App = require('./src/Main.elm');

window.runGenScene = function() {
    var node = document.getElementById("app");
    var app = App.Main.embed(node);

    console.log('runGenScene', window.jsGenScene, app);
}
