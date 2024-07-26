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
        const { importWithMap } = await import('https://unpkg.com/dynamic-importmap@0.1.0');
        const importMap = {
          imports: {
            "react": "https://esm.sh/react@18.2.0?dev",
            "react-dom": "https://esm.sh/react-dom@18.2.0?dev",
            "react-dom/client": "https://esm.sh/react-dom@18.2.0/client?dev",
          },
        };
        
        const React = await importWithMap("react", importMap);
        const { createRoot } = await importWithMap("react-dom/client", importMap);
        
        const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;

        const e = React.createElement;
        
        const jsDevMode = x.js_dev_mode;
        const customJsUrl = x.custom_js_url;
        const jsPackageVersion = (x.js_package_version ? x.js_package_version : "3.4.6");
        const pkgName = (jsDevMode ? "@vitessce/dev" : "vitessce");

        importMap.imports["vitessce"] = (customJsUrl && customJsUrl.length > 0
            ? customJsUrl
            : `https://unpkg.com/${pkgName}@${jsPackageVersion}`
        );

        const { Vitessce } = await importWithMap("vitessce", importMap);
        
        function VitessceWidget(props) {
            const { config, theme } = props;
            
            const divRef = React.useRef();
            
            const onConfigChange = React.useCallback((newConfig) => {
                if(window && window.Shiny && window.Shiny.setInputValue) {
                  Shiny.setInputValue("vitessce_on_config_change", newConfig);
                }
            }, [window.Shiny]);
            
            const vitessceProps = { height, theme, config, onConfigChange };
            return e('div', { ref: divRef, style: { height: height + 'px' } },
              e(React.Suspense, { fallback: e('div', {}, 'Loading...') },
                e(Vitessce, vitessceProps)
              )
            );
        }

        const root = createRoot(el);
        root.render(e(VitessceWidget, x));
      },
      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      }
    };
  }
});
