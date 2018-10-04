window.runGenScene = function() {
    var node = document.getElementById("app");
    var app = Elm.Main.embed(node);

    console.log('runGenScene', window.jsGenScene, app);
}
