#' Create a new Vitessce Config
#'
#' A helper function to construct a new `VitessceConfig` object.
#'
#' @param name A name for the view config.
#' @param description A description for the view config.
#' @return A new `VitessceConfig` object.
#'
#' @export
#'
#' @examples
#' vc <- vitessce_config("My config")
#' # On the returned object, access the methods of the VitessceConfig class.
#' ds <- vc$add_dataset("My dataset")
vitessce_config <- function(name = "", description = "") {
  vc <- VitessceConfig$new(name, description)
  vc
}

#' Create a new Vitessce Config from a list
#'
#' A helper function to construct a new `VitessceConfig` object based on an existing config in a list format.
#'
#' @param config A list containing a valid config.
#' @return A `VitessceConfig` object reflecting the list-based configuration values.
#'
#' @export
vitessce_config_from_list <- function(config) {
  vc <- VitessceConfig$new(config$name, config$description)
  for(d in config$datasets) {
    new_dataset <- vc$add_dataset(d$uid, d$name)
    for(f in d$files) {
      new_dataset$add_file(
        f$url,
        f$type,
        f$fileType
      )
    }
  }
  # TODO: Add each coordination scope from the incoming config.
  # TODO: Add the components (layout) from the incoming config.
  vc
}

#' Create a new Vitessce Config from a data object
#'
#' A helper function to construct a new `VitessceConfig` object based on an object containing single-cell or imaging data.
#'
#' @param obj An object from which to construct a config. Can be a SingleCellExperiment or Seurat object.
#' @param name A name for the view config.
#' @param description A description for the view config.
#' @return A `VitessceConfig` object containing the object as a member of the datasets list, with some automatically-configured views.
#'
#' @export
vitessce_config_from_object <- function(obj, name = NA, description = NA) {
  vc <- VitessceConfig$new(name, description)
  ds <- vc$add_dataset("From object")
  ds$add_object(obj)
  # TODO: infer views and coordinations
  vc
}

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