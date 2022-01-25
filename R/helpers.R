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

#' Merge file chunks
#'
#' @keywords internal
#' @param chunk_paths An ordered list of paths to each chunk to be merged.
#' @param out_path Where to store the merged file.
#' @return Nothing
merge_files <- function(chunk_paths, out_path) {
  binary_chunks <- lapply(chunk_paths, function(chunk_path) {
    fp <- chunk_path
    fp_size <- file.info(fp)$size
    fp_pointer <- file(fp, "rb")
    fp_data <- readBin(fp_pointer, what = "raw", n = fp_size)
    close(fp_pointer)
    return(fp_data)
  })

  fp <- out_path
  fp_pointer <- file(fp, "wb")
  for(binary_chunk in binary_chunks) {
    writeBin(binary_chunk, fp_pointer)
  }
  close(fp_pointer)
}

#' Merge JS file chunks
#'
#' @keywords internal
#' @param force Should the merging be done even if the merged file already exists? By default, FALSE.
#' @return Success.
merge_js <- function(force = FALSE) {
  pkg_name <- "vitessceR"
  dist_dir <- system.file(
    "inst", "htmlwidgets", "dist",
    package = pkg_name
  )
  if(dist_dir == "") {
    warning("Could not find inst/htmlwidgets/dist directory")
    return(FALSE)
  }
  js_bundle <- file.path(dist_dir, "index.js")
  map_bundle <- file.path(dist_dir, "index.js.map")
  merged_paths <- c(js_bundle, map_bundle)
  if(!force && all(file.exists(merged_paths))) {
    return(TRUE)
  }
  dist_files <- list.files(dist_dir)
  js_files <- file.path(dist_dir, dist_files[startsWith(dist_files, "index.js.chunk.")])
  map_files <- file.path(dist_dir, dist_files[startsWith(dist_files, "index.js.map.chunk.")])

  merge_files(js_files, js_bundle)
  merge_files(map_files, map_bundle)
  return(TRUE)
}




