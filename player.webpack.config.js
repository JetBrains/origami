const path = require('path');

// webpack.config.js
module.exports = {

    context: path.resolve(__dirname, '.'),

    entry: './player.js',

    output: {
      path: path.resolve(__dirname, '.'),
      filename: './player.min.js'
    },

    module: {
      noParse: [ /flat-surface-shader/, /src/, /build/ ],
      rules: [
        {
          test:    /\.elm$/,
          exclude: [ /elm-stuff/, /node_modules/, /build/ ],
          use: {
            loader: "elm-webpack-loader?verbose=true&warn=true"
          }
        },
        {
          test: /\.css$/,
          use: [ 'style-loader', 'css-loader' ]
        }
      ]
    },

    resolve: {
        // root: [
        //   path.join(__dirname, "..", "gulp", "node_modules"),
        //   path.join(__dirname, "..", "scripts", "modules"),
        // ],
        extensions: ['.js', '.elm']
        // modulesDirectories: [
        //   '/users/path/a/node_modules'
        // ]
    }

};
