#' Vitessce Config
#'
#' @param name A name for the view config.
#' @param description A description for the view config.
#'
#' @import R6
#'
#' @export
vitessceConfig <- function(name = "", description = "") {
  return;
}

#' Vitessce Config from list
#'
#' @param config A list containing a valid config.
#'
#' @export
vitessceConfigFromList <- function(config) {
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

#' Vitessce Config from object
#'
#' @param obj An object from which to construct a config. Can be a SingleCellExperiment or Seurat object.
#' @param name A name for the view config.
#' @param description A description for the view config.
#'
#' @export
vitessceConfigFromObject <- function(obj, name = NA, description = NA) {
  vc <- VitessceConfig$new(name, description)
  ds <- vc$add_dataset("From object")
  ds$add_object(obj)
  # TODO: infer views and coordinations
  vc
}
