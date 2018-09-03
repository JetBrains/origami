// webpack.config.js
module.exports = {
    entry: {
      app: [
        './index.js'
      ]
    },

    output: {
      filename: '[name].js',
    },

    module: {
      loaders: [
        {
          test: /\.svg$/,
          use: [ 'raw-loader' ]
        },
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: "elm-svg-loader"
            },
            {
              loader: "elm-webpack-loader?verbose=true&warn=true"
            }
          ]
        },
        {
          test: /\.css$/,
          use: [ 'style-loader', 'css-loader' ]
        }
      ]
    },

    devServer: {
      inline: true,
      stats: { colors: true },
    }
};
