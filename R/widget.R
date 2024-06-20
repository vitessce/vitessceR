
DEFAULT_PLUGIN_ESM <- "
function createPlugins(utilsForPlugins) {
    const {
        React,
        PluginFileType,
        PluginViewType,
        PluginCoordinationType,
        PluginJointFileType,
        z,
        useCoordination,
    } = utilsForPlugins;
    return {
        pluginViewTypes: undefined,
        pluginFileTypes: undefined,
        pluginCoordinationTypes: undefined,
        pluginJointFileTypes: undefined,
    };
}
export default { createPlugins };
"

ESM <- "
import { importWithMap } from 'https://unpkg.com/dynamic-importmap@0.1.0';
const importMap = {
  imports: {
    'react': 'https://esm.sh/react@18.2.0?dev',
    'react-dom': 'https://esm.sh/react-dom@18.2.0?dev',
    'react-dom/client': 'https://esm.sh/react-dom@18.2.0/client?dev',
  },
};

const React = await importWithMap('react', importMap);
const { createRoot } = await importWithMap('react-dom/client', importMap);

const e = React.createElement;

const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;

function prependBaseUrl(config, proxy, hasHostName) {
  return config;
}

async function render(view) {

  const cssUid = view.model.get('uid');
  const jsDevMode = view.model.get('js_dev_mode');
  const jsPackageVersion = view.model.get('js_package_version');
  const customJsUrl = view.model.get('custom_js_url');
  const pluginEsm = view.model.get('plugin_esm');
  const remountOnUidChange = view.model.get('remount_on_uid_change');
  const storeUrls = view.model.get('store_urls');

  const pkgName = (jsDevMode ? '@vitessce/dev' : 'vitessce');

  importMap.imports.vitessce = (customJsUrl.length > 0
    ? customJsUrl
    : `https://unpkg.com/${pkgName}@${jsPackageVersion}`
  );

  const {
    Vitessce,
    PluginFileType,
    PluginViewType,
    PluginCoordinationType,
    PluginJointFileType,
    z,
    useCoordination,
  } = await importWithMap('vitessce', importMap);

  let pluginViewTypes;
  let pluginCoordinationTypes;
  let pluginFileTypes;
  let pluginJointFileTypes;

  const stores = {};
  /*
  const stores = Object.fromEntries(
      storeUrls.map(storeUrl => ([
          storeUrl,
          {
              async get(key) {
                  const [data, buffers] = await view.experimental.invoke('_zarr_get', [storeUrl, key]);
                  if (!data.success) return undefined;
                  return buffers[0].buffer;
              },
          }
      ])),
  );
  */

  try {
      const pluginEsmUrl = URL.createObjectURL(new Blob([pluginEsm], { type: 'text/javascript' }));
      const pluginModule = (await import(pluginEsmUrl)).default;
      URL.revokeObjectURL(pluginEsmUrl);

      const pluginsObj = await pluginModule.createPlugins({
          React,
          PluginFileType,
          PluginViewType,
          PluginCoordinationType,
          PluginJointFileType,
          z,
          useCoordination,
      });
      pluginViewTypes = pluginsObj.pluginViewTypes;
      pluginCoordinationTypes = pluginsObj.pluginCoordinationTypes;
      pluginFileTypes = pluginsObj.pluginFileTypes;
      pluginJointFileTypes = pluginsObj.pluginJointFileTypes;
  } catch(e) {
      console.error(e);
  }


   function VitessceWidget(props) {
      const { model } = props;

      const [config, setConfig] = React.useState(prependBaseUrl(model.get('config'), model.get('proxy'), model.get('has_host_name')));
      const [validateConfig, setValidateConfig] = React.useState(true);
      const height = model.get('height');
      const theme = model.get('theme') === 'auto' ? (prefersDark ? 'dark' : 'light') : model.get('theme');

      const divRef = React.useRef();

      React.useEffect(() => {
          if(!divRef.current) {
              return () => {};
          }

          function handleMouseEnter() {
              const jpn = divRef.current.closest('.jp-Notebook');
              if(jpn) {
                  jpn.style.overflow = 'hidden';
              }
          }
          function handleMouseLeave(event) {
              if(event.relatedTarget === null || (event.relatedTarget && event.relatedTarget.closest('.jp-Notebook')?.length)) return;
              const jpn = divRef.current.closest('.jp-Notebook');
              if(jpn) {
                  jpn.style.overflow = 'auto';
              }
          }
          divRef.current.addEventListener('mouseenter', handleMouseEnter);
          divRef.current.addEventListener('mouseleave', handleMouseLeave);

          return () => {
              if(divRef.current) {
                  divRef.current.removeEventListener('mouseenter', handleMouseEnter);
                  divRef.current.removeEventListener('mouseleave', handleMouseLeave);
              }
          };
      }, [divRef]);

      // Config changed on JS side (from within <Vitessce/>),
      // send updated config to Python side.
      const onConfigChange = React.useCallback((config) => {
          model.set('config', config);
          setValidateConfig(false);
          model.save_changes();
      }, [model]);

      // Config changed on Python side,
      // pass to <Vitessce/> component to it is updated on JS side.
      React.useEffect(() => {
          model.on('change:config', () => {
              const newConfig = prependBaseUrl(model.get('config'), model.get('proxy'), model.get('has_host_name'));

              // Force a re-render and re-validation by setting a new config.uid value.
              // TODO: make this conditional on a parameter from Python.
              //newConfig.uid = `random-${Math.random()}`;
              //console.log('newConfig', newConfig);
              setConfig(newConfig);
          });
      }, []);

      const vitessceProps = {
          height, theme, config, onConfigChange, validateConfig,
          pluginViewTypes, pluginCoordinationTypes, pluginFileTypes, pluginJointFileTypes,
          remountOnUidChange, stores,
      };

      return e('div', { ref: divRef, style: { height: height + 'px' } },
          e(React.Suspense, { fallback: e('div', {}, 'Loading...') },
              e(React.StrictMode, {},
                  e(Vitessce, vitessceProps)
              ),
          ),
      );
  }

  const root = createRoot(view.el);
  root.render(e(VitessceWidget, { model: view.model }));

  return () => {
      // Re-enable scrolling.
      const jpn = view.el.closest('.jp-Notebook');
      if(jpn) {
          jpn.style.overflow = 'auto';
      }

      // Clean up React and DOM state.
      root.unmount();
      if(view._isFromDisplay) {
          view.el.remove();
      }
  };
}
export default { render };
"


#' Vitessce Widget
#'
#' This function creates a new Vitessce htmlwidget.
#' A Vitessce widget is defined by a config which specifies dataset(s),
#' a layout of views, and linked view connections. A config object can be
#' passed to the widget using the \code{config} parameter.
#'
#' We do not recommend calling this function directly. Instead, we
#' recommend calling the \code{widget()} method on the \code{VitessceConfig}
#' instance.
#'
#' @param config A view config as a `VitessceConfig` object.
#' @param theme The theme of the widget, either "dark" or "light". Optional. By default, "dark".
#' @param width The width of the widget as a number or CSS string. Optional.
#' @param height The height of the widget as a number or CSS string. Optional.
#' @param port The port for the local web server (which serves local dataset objects to the widget).
#' Optional. By default, uses open port between 8000 and 9000.
#' @param base_url The base URL for the web server. Optional.
#' By default, creates a localhost URL which includes the port.
#' @param serve Should local data be served by running a local web server with R plumber? By default, TRUE.
#' @param element_id An element ID. Optional.
#' @return The htmlwidget.
#'
#' @export
#'
#' @examples
#' vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
#' vc$widget()
vitessce_widget <- function(config, theme = "dark", width = NA, height = NA, port = NA, base_url = NA, serve = TRUE, element_id = NULL) {

  use_port <- port
  if(is.na(port)) {
    use_port <- httpuv::randomPort(min = 8000, max = 9000, n = 1000)
  }
  use_base_url <- paste0("http://localhost:", use_port)
  if(!is.na(base_url)) {
    use_base_url <- base_url
  }

  server <- VitessceConfigServer$new(use_port)
  config_list <- config$to_list(base_url = use_base_url)
  routes <- config$get_routes()
  server$create_routes(routes)

  # run the web server if necessary
  if(length(routes) > 0 && serve) {
    # run in a background process
    future::plan(future::multisession)
    f <- future::future(server$run())
    on.exit({
      try({
        stop_future(f)
      }, silent = TRUE)
    })
  }

  # forward widget options to javascript
  params = list(
    config = config_list,
    theme = theme
  )

  # create widget
  anyhtmlwidget::AnyHtmlWidget$new(
    .esm = ESM,
    .mode = "static",
    .width = width,
    .height = height,
    height = 600,
    count = 1,
    config = config_list,
    theme = theme,
    uid = 'todo-uuid-here',
    proxy = FALSE,
    js_package_version = '3.4.6',
    js_dev_mode = FALSE,
    custom_js_url = '',
    plugin_esm = DEFAULT_PLUGIN_ESM,
    remount_on_uid_change = TRUE
  )
}
