const path = require('path');
const fs = require('fs');
const PnpWebpackPlugin = require('pnp-webpack-plugin');

const mode = 'development';
process.env.NODE_ENV = mode;

// Custom webpack rules are generally the same for all webpack bundles, hence
// stored in a separate local variable.
const rules = [
    {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
            loader: require.resolve('babel-loader'),
            options: {
                customize: require.resolve(
                  'babel-preset-react-app/webpack-overrides'
                ),
            }
        },
    },
    {
        test: /\.css$/,
        use: [
            {
                loader: require.resolve('style-loader'),
            },
            {
                loader: require.resolve('css-loader'),
            },
        ],
    },
];

const resolve = {
  alias: {
    'txml/txml': 'txml/dist/txml',
  },
};

const resolveLoader = {
    plugins: [
      // Also related to Plug'n'Play, but this time it tells webpack to load its loaders
      // from the current package.
      PnpWebpackPlugin.moduleLoader(module),
    ],
};

module.exports = [
    {
        mode,
        entry: './inst/htmlwidgets/lib/index.js',
        output: {
            filename: 'index.js',
            path: path.resolve(__dirname, 'inst', 'htmlwidgets', 'dist'),
            libraryTarget: 'umd'
        },
        devtool: 'cheap-source-map',
        module: {
            rules: rules
        },
        resolve,
        resolveLoader,
    },
];
