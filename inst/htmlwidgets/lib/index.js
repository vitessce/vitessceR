import React from 'react';
import ReactDOM from 'react-dom';
import { Vitessce } from 'vitessce';
import 'vitessce/dist/es/production/static/css/index.css';
import './index.css';


export function renderVitessce(el, config, width, height, theme, port) {
    function onConfigChange(nextConfig) {
        fetch(`http://localhost:${port}/config`, {
            method: 'POST',
            mode: 'cors',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(nextConfig)
        });
    }
    ReactDOM.render(
        <Vitessce
            config={config}
            height={height}
            theme={theme}
            onConfigChange={onConfigChange}
        />,
        el
    );
}