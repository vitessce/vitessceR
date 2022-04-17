#' Horizontally concatenate views
#'
#' A helper function to construct a new `VitessceConfigViewHConcat` object based on multiple views.
#'
#' @param ... A variable number of `VitessceConfigView`, `VitessceConfigViewHConcat`, or `VitessceConfigViewVConcat` objects.
#' @return A `VitessceConfigViewHConcat` object.
#'
#' @export
#' @examples
#' vc <- VitessceConfig$new("My config")
#' ds <- vc$add_dataset("My dataset")
#' spatial <- vc$add_view(ds, Component$SPATIAL)
#' gene_list <- vc$add_view(ds, Component$GENES)
#' vc$layout(hconcat(spatial, gene_list))
#' vc$widget()
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
#' @examples
#' vc <- VitessceConfig$new("My config")
#' ds <- vc$add_dataset("My dataset")
#' spatial <- vc$add_view(ds, Component$SPATIAL)
#' gene_list <- vc$add_view(ds, Component$GENES)
#' vc$layout(vconcat(spatial, gene_list))
#' vc$widget()
vconcat <- function(...) {
  vcvvc <- VitessceConfigViewVConcat$new(list(...))
  vcvvc
}

#' Create an empty named list
#'
#' A helper function to construct an empty list which converts to a JSON object rather than a JSON array.
#'
#' @keywords internal
#' @param ... A variable number of list entries.
#' @return An empty named list.
#'
#' @export
#' @examples
#' default_window <- obj_list(
#'   min = 0,
#'   max = 255
#' )
obj_list <- function(...) {
  retval <- stats::setNames(list(), character(0))
  param_list <- list(...)
  for(key in names(param_list)) {
    retval[[key]] = param_list[[key]]
  }
  retval
}

#' Check if a value, potentially a vector, is NA
#'
#' @keywords internal
#' @param val The value to check
#' @return Whether the value is NA
is_na <- function(val) {
  if(length(val) > 1) {
    return(FALSE)
  } else {
    return(is.na(val))
  }
}

#' Try to stop a future
#'
#' @keywords internal
#' @param f The future to stop
#' @return Nothing
stop_future <- function(f) {
  # Reference: https://github.com/HenrikBengtsson/future/issues/93#issuecomment-349625087
  if(!is.null(f$job) && Sys.getpid() != f$job$pid) {
    tools::pskill(f$job$pid, signal = tools::SIGTERM)
    tools::pskill(f$job$pid, signal = tools::SIGKILL)
  }
}
