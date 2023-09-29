

make_unique_filename <- function(file_ext) {
  return(paste0(uuid::UUIDgenerate(output = "string"), file_ext))
}


file_path_to_url_path <- function(local_path, prepend_slash = TRUE) {
  if(grepl("\\", local_path, fixed = TRUE)) {
    segments <- stringr::str_split(local_path, "\\\\")[[1]]
    url_path <- paste(segments, collapse = "/")
  } else {
    url_path <- local_path
  }
  if(prepend_slash && substr(url_path, 1, 1) != "/") {
    url_path <- paste0("/", url_path)
  }
  return(url_path)
}

#' Abstract dataset object wrapper class
#' @title AbstractWrapper Class
#' @docType class
#' @description
#' Abstract class representing a local dataset object in a Vitessce dataset.
#'
#' @rdname AbstractWrapper
#' @export
AbstractWrapper <- R6::R6Class("AbstractWrapper",
  public = list(
    #' @field out_dir The directory for processed output files.
    #' @keywords internal
    out_dir = NULL,
    #' @field use_cache If converted output files already exist, should they be used?
    #' @keywords internal
    use_cache = NULL,
    #' @field is_remote Is the data object fully remote?
    #' @keywords internal
    is_remote = NULL,
    #' @field routes A list of `VitessceConfigServerRoute` objects.
    #' @keywords internal
    routes = NULL,
    #' @field file_def_creators A list of file definition creator functions.
    #' @keywords internal
    file_def_creators = NULL,
    #' @field base_dir The base directory for local data.
    #' @keywords internal
    base_dir = NULL,
    #' @description
    #' Create an abstract wrapper around a data object.
    #' @param out_dir The directory for processed output files.
    #' @param use_cache If converted output files already exist, should they be used? By default, FALSE.
    #' @return A new `AbstractWrapper` object.
    initialize = function(out_dir = NA, use_cache = FALSE) {
      if(!is.na(out_dir)) {
        self$out_dir <- out_dir
      } else {
        self$out_dir <- tempdir()
      }
      self$is_remote <- FALSE
      self$use_cache <- use_cache
      self$routes <- list()
      self$file_def_creators <- list()
      self$base_dir <- NA
    },
    #' @description
    #' Fill in the file_def_creators array.
    #' Each function added to this list should take in a base URL and generate a Vitessce file definition.
    #' If this wrapper is wrapping local data, then create routes and fill in the routes array.
    #' This method is void, should not return anything.
    #'
    #' @param dataset_uid A unique identifier for this dataset.
    #' @param obj_i Within the dataset, the index of this data wrapper object.
    #' @param base_dir Path to a base directory.
    convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
      dir.create(self$get_out_dir(dataset_uid, obj_i), recursive = TRUE, showWarnings = FALSE)
      self$base_dir <- base_dir
    },
    #' @description
    #' Obtain the routes that have been created for this wrapper class.
    #'
    #' @return A list of server routes.
    get_routes = function() {
      return(self$routes)
    },
    #' @description
    #' Obtain the file definitions for this wrapper class.
    #'
    #' @param base_url A base URL to prepend to relative URLs.
    #' @return A list of server routes.
    get_file_defs = function(base_url) {
      file_defs_with_base_url <- list()
      for(file_def_creator in self$file_def_creators) {
        file_def <- file_def_creator(base_url)
        if(is.list(file_def)) {
          file_defs_with_base_url <- append(file_defs_with_base_url, list(file_def))
        }
      }
      return(file_defs_with_base_url)
    },
    #' @description
    #' Create a web server route for this object.
    #'
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A new `VitessceConfigServerStaticRoute` instance.
    get_out_dir_route = function(dataset_uid, obj_i) {
      route <- VitessceConfigServerStaticRoute$new(
        self$get_route_str(dataset_uid, obj_i),
        self$get_out_dir(dataset_uid, obj_i)
      )
      return(route)
    },
    get_local_dir_url = function(base_url, dataset_uid, obj_i, local_dir_path, local_dir_uid) {
      if(!self$is_remote && !is.na(self$base_dir)) {
        return(self$get_url_simple(base_url, file_path_to_url_path(local_dir_path, prepend_slash = FALSE)))
      }
      return(self$get_url(base_url, dataset_uid, obj_i, local_dir_uid))
    },
    get_local_file_url = function(base_url, dataset_uid, obj_i, local_file_path, local_file_uid) {
      # Same logic as get_local_dir_url
      return(self$get_local_dir_url(base_url, dataset_uid, obj_i, local_file_path, local_file_uid))
    },
    get_local_dir_route = function(dataset_uid, obj_i, local_dir_path, local_dir_uid) {
      if(!self$is_remote) {
        if(is.na(self$base_dir)) {
          route_path <- self$get_route_str(dataset_uid, obj_i, local_dir_uid)
        } else {
          route_path <- file_path_to_url_path(local_dir_path)
          local_dir_path <- file.path(self$base_dir, local_dir_path)
        }
        route <- VitessceConfigServerStaticRoute$new(
          route_path,
          local_dir_path
        )
        return(list(route))
      }
      return(list())
    },
    get_local_file_route = function(dataset_uid, obj_i, local_file_path, local_file_uid) {
      if(!self$is_remote) {
        if(is.na(self$base_dir)) {
          route_path <- self$get_route_str(dataset_uid, obj_i, local_file_uid)
          local_file_path <- local_file_path
        } else {
          # Has base_dir
          route_path <- file_path_to_url_path(local_file_path)
          local_file_path <- file.path(self$base_dir, local_file_path)
        }
        route <- VitessceConfigServerFileRoute$new(
          route_path,
          local_file_path
        )
        return(list(route))
      }
      return(list())
    },
    #' @description
    #' Create a local web server URL for a dataset object.
    #' @param base_url The base URL on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @param ... Passes extra arguments to `get_route_str`
    #' @return A URL as a string.
    get_url = function(base_url, dataset_uid, obj_i, ...) {
        retval <- paste0(base_url, self$get_route_str(dataset_uid, obj_i, ...))
        return(retval)
    },
    get_url_simple = function(base_url, suffix) {
      return(paste0(base_url, "/", suffix))
    },
    #' @description
    #' Create a string representing a web server route path (the part following the base URL).
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @param ... Pastes extra arguments together after `obj_i`, separated by slash.
    #' @return A path as a string.
    get_route_str = function(dataset_uid, obj_i, ...) {
      retval <- paste0("/", paste(dataset_uid, obj_i, ..., sep = "/"))
      return(retval)
    },
    #' @description
    #' Create a directory path to a dataset within the base output directory.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @param ... Passes extra arguments to `file.path`
    #' @return A path as a string.
    get_out_dir = function(dataset_uid, obj_i, ...) {
      retval <- file.path(self$out_dir, dataset_uid, obj_i, ...)
      return(retval)
    },
    #' @description
    #' Automatically configure views for a particular dataset.
    #' @param vc A `VitessceConfig` instance to configure.
    auto_view_config = function(vc) {
        warning("Auto view configuration has not yet been implemented for this data object wrapper class.")
    }
  )
)
