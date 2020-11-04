#' Create a new Vitessce Config
#'
#' A helper function to construct a new `VitessceConfig` object.
#'
#' @param name A name for the view config.
#' @param description A description for the view config.
#'
#' @import R6
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
#'
#' @export
vitessce_config_from_object <- function(obj, name = NA, description = NA) {
  vc <- VitessceConfig$new(name, description)
  ds <- vc$add_dataset("From object")
  ds$add_object(obj)
  # TODO: infer views and coordinations
  vc
}
