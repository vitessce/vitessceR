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
#'
#' @export
#'
#' @examples
#' vc <- VitessceConfig$new("My config")
#' vc$widget()
vitessce_widget <- function(config, theme = "dark", width = NULL, height = NULL, port = NA, base_url = NA, serve = TRUE, element_id = NULL) {

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
    future::future(server$run())
  }

  # forward widget options to javascript
  params = list(
    config = config_list,
    theme = theme
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'vitessce',
    params,
    width = width,
    height = height,
    package = 'vitessce',
    elementId = element_id
  )
}

#' Shiny bindings for vitessce
#'
#' Output and render functions for using vitessce within Shiny
#' applications and interactive Rmd documents.
#'
#' @param output_id output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a vitessce
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @rdname vitessce-shiny
#' @export
vitessce_output <- function(output_id, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(output_id, 'vitessce', width, height, package = 'vitessce')
}

#' @name vitessce-shiny
#' @export
render_vitessce <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, vitessce_output, env, quoted = TRUE)
}
