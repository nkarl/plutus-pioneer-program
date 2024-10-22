const Path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const NodePolyfillPlugin = require('node-polyfill-webpack-plugin');
const { ProvidePlugin } = require('webpack');
const dotenv = require('dotenv-webpack');

module.exports = {
    experiments: {
        asyncWebAssembly: true,
        topLevelAwait: true,
    },
    mode: 'development',
    entry: './src/index.js',
    devtool: 'inline-source-map',
    devServer: {
        static: './dist',
        port: 9081,
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: 'Vesting',
            template: './src/index.html'
        }),
        new CopyWebpackPlugin({
            patterns: [{
                from: 'static'
            }]
        }),
        new NodePolyfillPlugin(),
        new ProvidePlugin({
            process: 'process/browser'
        }),
        new dotenv(),
    ],
    output: {
        filename: 'main.js',
        path: Path.resolve(__dirname, 'dist'),
        clean: true,
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ],
    },
    resolve: {
        alias: {
            'jquery': Path.join(__dirname, 'node_modules/jquery/src/jquery')
        },
    }
};

