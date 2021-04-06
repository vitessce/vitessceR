import React from 'react';
import ReactDOM from 'react-dom';
import { Vitessce } from 'vitessce';
import 'vitessce/dist/es/production/static/css/index.css';
import './index.css';

var globalThis = window;
console.log(globalThis);

export function render_vitessce(el, config, width, height, theme) {
    ReactDOM.render(
        <Vitessce
            config={config}
            height={height}
            theme={theme}
        />,
        el
    );
}
