#' Export a Vitessce configuration to a directory of static files.
#'
#' A helper function to export files associated with a particular Vitessce configuration to a local directory.
#' We do not recommend calling this function directly. Instead, use the `export()` function on a `VitessceConfig` instance.
#'
#' @param config An instance of `VitessceConfig`
#' @param out_dir The directory for storing exported files.
#' @examples
#' vc <- VitessceConfig$new("My config")
#' dataset <- vc$add_dataset("My dataset")
#' description <- vc$add_view(dataset, Component$DESCRIPTION)
#' vc$layout(description)
#' vc$export(to = "files", out_dir = "./my_exported_files")
export_to_files <- function(config, out_dir) {
  routes <- config$get_routes()

  for(route in routes) {
    # Get the intended web server route, removing the initial "/"
    route_path <- substr(route$path, 2, stringr::str_length(route$path))
    out_path <- file.path(out_dir, route_path)

    dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

    if(class(route)[1] == "VitessceConfigServerStaticRoute") {
      static_dir <- route$directory
      files_to_copy = list.files(static_dir, full.names = TRUE)
      file.copy(files_to_copy, out_path, recursive = TRUE)
    } else {
      warning("Unknown route class found in export_to_files.")
    }
  }
}
