#' CSV file wrapper class
#' @title CsvWrapper Class
#' @docType class
#' @description
#' Class representing a CSV file in a Vitessce dataset.
#'
#' @rdname CsvWrapper
#' @export
CsvWrapper <- R6::R6Class("CsvWrapper",
  inherit = AbstractWrapper,
  public = list(
    #' @field csv_path The object to wrap.
    #' @keywords internal
    csv_path = NULL,
    #' @field csv_url The object to wrap.
    #' @keywords internal
    csv_url = NULL,
    #' @field local_csv_uid The path to the local zarr store.
    #' @keywords internal
    local_csv_uid = NULL,

    #' @field data_type The Vitessce data type for this file.
    #' @keywords internal
    data_type = NULL,
    #' @field options A list of options to pass to the Vitessce file definition.
    #' @keywords internal
    options=NULL,
    #' @field coordination_values A list of coordination values to pass to the Vitessce file definition.
    #' @keywords internal
    coordination_values=NULL,
    #' @field request_init A list of requestInit values to pass to fetch when loading the CSV over HTTP.
    #' @keywords internal
    request_init = NULL,
    #' @description
    #' Create a wrapper around a CSV file.
    #' @param csv_path The path to a local CSV file.
    #' @param csv_url The URL to a remote CSV file.
    #' @param data_type The Vitessce data type for this file.
    #' @param options A list of options to pass to the Vitessce file definition.
    #' @param coordination_values A list of coordination values to pass to the Vitessce file definition.
    #' @param request_init A list of requestInit values to pass to fetch when loading the CSV over HTTP.
    #' @param ... Parameters inherited from `AbstractWrapper`.
    #' @return A new `CsvWrapper` object.
    initialize = function(csv_path = NA, csv_url = NA, data_type = NA, options = NA, coordination_values = NA, request_init = NA, ...) {
      super$initialize(...)
      self$csv_path <- csv_path
      self$csv_url <- csv_url

      if(is_na(data_type)) {
        stop("Expected data_type to be provided.")
      }

      if(!is.na(csv_url) && !is.na(csv_path)) {
        stop("Did not expect csv_url to be provided with csv_path.")
      }

      if(is.na(csv_url) && is.na(csv_path)) {
        stop("Expected either csv_url or csv_path to be provided.")
      }

      if(!is.na(csv_path)) {
        self$is_remote <- FALSE
      } else {
        self$is_remote <- TRUE
      }

      self$local_csv_uid <- make_unique_filename(".csv")

      self$data_type <- data_type
      self$options <- options
      self$coordination_values <- coordination_values
      self$request_init <- request_init
    },
    #' @description
    #' Create the JSON output files, web server routes, and file definition creators.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @param base_dir A base directory for local data.
    convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
      if(!self$is_remote) {
        super$convert_and_save(dataset_uid, obj_i, base_dir = base_dir)
      }

      # Get the file definition creator functions.
      file_def_creator <- self$make_file_def_creator(dataset_uid, obj_i)

      # Append the new file definition creators functions to the main list.
      self$file_def_creators <- append(self$file_def_creators, file_def_creator)

      # Create a web server route object for the directory of JSON files.
      new_routes <- self$make_routes(dataset_uid, obj_i)
      for(route in new_routes) {
        self$routes <- append(self$routes, route)
      }
    },
    #' @description
    #' Get a list of server route objects.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    make_routes = function(dataset_uid, obj_i) {
      return(self$get_local_file_route(dataset_uid, obj_i, self$csv_path, self$local_csv_uid))
    },
    #' @description
    #' Get the URL to the Zarr store, to fill in the file URL in the file definitions.
    #' @param base_url The base URL, on which the route will be served.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @keywords internal
    #' @return A URL as a string.
    get_csv_url = function(base_url, dataset_uid, obj_i) {
      if(self$is_remote) {
        return(self$csv_url)
      } else {
        return(self$get_local_file_url(base_url, dataset_uid, obj_i, self$csv_path, self$local_csv_uid))
      }
    },
    #' @description
    #' Make the file definition creator function for the cells data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_file_def_creator = function(dataset_uid, obj_i) {
      get_csv <- function(base_url) {
        file_def <- list(
          fileType = paste0(self$data_type, ".csv"),
          url = self$get_csv_url(base_url, dataset_uid, obj_i)
        )
        if(!is_na(self$options)) {
          file_def[['options']] <- self$options
        }
        if(!is_na(self$request_init)) {
          file_def[['requestInit']] <- self$request_init
        }
        if(!is_na(self$coordination_values)) {
          file_def[['coordinationValues']] <- self$coordination_values
        }
        return(file_def)
      }
      return(get_csv)
    }
  ),
)
