

#' Horizontally concatenate views
#'
#' A helper function to construct a new `VitessceConfigViewHConcat` object based on multiple views.
#'
#' @param ... A variable number of `VitessceConfigView`, `VitessceConfigViewHConcat`, or `VitessceConfigViewVConcat` objects.
#' @return A `VitessceConfigViewHConcat` object.
#'
#' @export
hconcat <- function(...) {
  vcvhc <- VitessceConfigViewHConcat$new(list(...))
  vcvhc
}

#' Vertically concatenate views
#'
#' A helper function to construct a new `VitessceConfigViewVConcat` object based on multiple views.
#'
#' @param ... A variable number of `VitessceConfigView`, `VitessceConfigViewHConcat`, or `VitessceConfigViewVConcat` objects.
#' @return A `VitessceConfigViewVConcat` object.
#'
#' @export
vconcat <- function(...) {
  vcvvc <- VitessceConfigViewVConcat$new(list(...))
  vcvvc
}

#' Create an empty named list
#'
#' A helper function to construct an empty list which converts to a JSON object rather than a JSON array.
#'
#' @return An empty named list.
#' @keywords internal
obj_list <- function() {
  retval <- setNames(list(), character(0))
  retval
}
