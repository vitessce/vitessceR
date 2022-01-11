#' Export a Vitessce configuration to a directory of static files.
#'
#' A helper function to export files associated with a particular Vitessce configuration to a local directory.
#' We do not recommend calling this function directly. Instead, use the `export()` function on a `VitessceConfig` instance.
#'
#' @keywords internal
#' @param config An instance of `VitessceConfig`
#' @param with_config Should the config be saved in the `out_dir` (as a JSON file)?
#' @param base_url If `with_config` is TRUE, what `base_url` value should be used for creation of the JSON config?
#' @param out_dir The directory for storing exported files.
#' @returns If `with_config = TRUE`, returns the config after calling `to_list` with the specified `base_url`.
#' @examples
#' vc <- VitessceConfig$new("My config")
#' dataset <- vc$add_dataset("My dataset")
#' description <- vc$add_view(dataset, Component$DESCRIPTION)
#' vc$layout(description)
#' vc$export(to = "files", out_dir = "./data")
export_to_files <- function(config, with_config, base_url, out_dir) {
  routes <- config$get_routes()

  for(route in routes) {
    # Get the intended web server route, removing the initial "/"
    route_path <- substr(route$path, 2, stringr::str_length(route$path))
    out_path <- file.path(out_dir, route_path)

    dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
    # Copy the converted files from their original directory to the `out_dir`.
    static_dir <- route$directory
    files_to_copy = list.files(static_dir, full.names = TRUE)
    file.copy(files_to_copy, out_path, recursive = TRUE)
  }

  if(with_config) {
    # When `with_config` is true, we need to save the view config to a new JSON file.
    out_path <- file.path(out_dir, "config.json")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    vc_list <- config$to_list(base_url = base_url)
    vc_json <- jsonlite::toJSON(vc_list, auto_unbox = TRUE)
    write(vc_json, file = out_path)
    return(vc_list)
  }
}
