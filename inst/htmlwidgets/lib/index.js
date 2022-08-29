import React from 'react';
import ReactDOM from 'react-dom';
import { Vitessce } from 'vitessce';
import allSettled from 'promise.allsettled';
import 'vitessce/dist/es/production/static/css/index.css';
import './index.css';

if((new URLSearchParams(window.location.search)).get("viewer_pane") === "1") {
    // This Vitessce instance is currently running in the Viewer pane of RStudio.
    // The built-in browser does not support all modern JavaScript features used by Vitessce.
    // Click the 'Show in new window' button to run in a standalone web browser if the polyfill does not work.
    console.log("Detected running in the RStudio Viewer pane which does not implement Promise.allSettled. Using polyfill.");
    allSettled.shim();
}

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
