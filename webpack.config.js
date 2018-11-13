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
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/, /build/],
          use: [
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
      // public: 'code2art.labs.jb.gg',
      stats: { colors: true },
      disableHostCheck: true,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
        "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization"
      }
      // host: '0.0.0.0',
      // port: 80
    }
};
