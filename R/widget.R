#' Vitessce Widget
#'
#' The \code{vitessceWidget} function creates a new Vitessce htmlwidget.
#' A Vitessce widget is defined by a view config which specifies dataset(s),
#' a layout of views, and linked view connections. A config object can be
#' passed to the widget using the \code{config} parameter.
#'
#' @param config A view config as a `VitessceConfig` object.
#' @param width The width of the widget as a number or CSS string.
#' @param height The height of the widget as a number or CSS string.
#'
#' @import htmlwidgets
#'
#' @export
vitessceWidget <- function(config = NULL, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = list(
    config = config
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'vitessce',
    x,
    width = width,
    height = height,
    package = 'vitessce',
    elementId = elementId
  )
}

#' Shiny bindings for vitessce
#'
#' Output and render functions for using vitessce within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
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
vitessceOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'vitessce', width, height, package = 'vitessce')
}

#' @rdname vitessce-shiny
#' @export
renderVitessce <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, vitessceOutput, env, quoted = TRUE)
}
