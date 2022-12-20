function asEsModule(component) {
  return {
    __esModule: true,
    default: component,
  };
}

HTMLWidgets.widget({
  name: 'vitessceR',
  type: 'output',
  factory: function(el, width, height) {
    // TODO: define shared variables for this instance

    return {
      renderValue: async function(x) {
        const React = await import('https://unpkg.com/es-react@16.13.1/react.js');
        const ReactDOM = await import('https://unpkg.com/es-react@16.13.1/react-dom.js');
        const d3 = await import("https://esm.sh/d3-require@1.3.0");
        
        const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;

        const e = React.createElement;
        let customRequire = d3.require;
        
        const jsPackageVersion = "2.0.2"; // TODO
        
        const customJsUrl = x.custom_js_url;
        if(customJsUrl && customJsUrl.length > 0) {
            customRequire = d3.requireFrom(async () => {
                return customJsUrl;
            });
        }
        const aliasedRequire = customRequire.alias({
            "react": React,
            "react-dom": ReactDOM
        });
        const Vitessce = React.lazy(() => aliasedRequire(`vitessce@${jsPackageVersion}`).then(vitessce => asEsModule(vitessce.Vitessce)));

        // console.log(d3);
        // console.log(e);
        // console.log(x);
        
        function VitessceWidget(props) {
            const { config, theme } = props;
            
            const divRef = React.useRef();
            
            const onConfigChange = React.useCallback((newConfig) => {
                if(window && window.Shiny && window.Shiny.setInputValue) {
                  Shiny.setInputValue("on_config_change", newConfig);
                }
            }, [window.Shiny]);
            
            const vitessceProps = { height, theme, config, onConfigChange };
            return e('div', { ref: divRef, style: { height: height + 'px' } },
                e('style', {}, `
                /* To undo the bootstrap font-sizing from Shiny. */
                html { font-size: initial; }
                `),
                e(React.Suspense, { fallback: e('div', {}, 'Loading...') },
                    e(Vitessce, vitessceProps)
                )
            );
        }
        
        ReactDOM.render(e(VitessceWidget, x), el);
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
