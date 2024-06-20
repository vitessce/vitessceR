
ESM <- "
function render({ el, model }) {
  console.log(model.get('config'));
  let count = () => model.get('count');
  let btn = document.createElement('button');
  btn.innerHTML = `count button ${count()}`;
  btn.addEventListener('click', () => {
    model.set('count', count() + 1);
    model.save_changes();
  });
  model.on('change:count', () => {
        btn.innerHTML = `count is ${count()}`;
  });
  el.appendChild(btn);
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
    count = 1,
    config = config_list,
    theme = theme
  )
}
