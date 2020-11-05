#' Vitessce Widget
#'
#' This function creates a new Vitessce htmlwidget.
#' A Vitessce widget is defined by a config which specifies dataset(s),
#' a layout of views, and linked view connections. A config object can be
#' passed to the widget using the \code{config} parameter.
#'
#' @param config A view config as a `VitessceConfig` object.
#' @param theme The theme of the widget, either "dark" or "light". Optional. By default, "dark".
#' @param width The width of the widget as a number or CSS string. Optional.
#' @param height The height of the widget as a number or CSS string. Optional.
#' @param port The port for the local web server (which serves local dataset objects to the widget). Optional. By default, 8000.
#' @param element_id An element ID. Optional.
#'
#' @import htmlwidgets
#' @import rjson
#' @import plumber
#'
#' @export
#'
#' @examples
#' vc <- vitessce_config("My config")
#' vitessce_widget(vc)
vitessce_widget <- function(config, theme = "dark", width = NULL, height = NULL, port = 8000, element_id = NULL) {
  server <- VitessceConfigServer$new(port)
  on_obj <- server$on_obj

  config_list = config$to_list(on_obj)

  # run the web server if necessary
  if(server$num_obj > 0) {
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
