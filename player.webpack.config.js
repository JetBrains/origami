const path = require('path');

const config = {

    // mode: 'development',
    mode: 'production',

    entry: path.join(__dirname, 'player.js'),

    output: {
      path: path.join(__dirname, '.'),
      filename: './player.bundle.js'
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
        extensions: ['.js']
    }

};

module.exports = config;
