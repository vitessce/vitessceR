#' OME-Zarr object wrapper classes
#' Classes representing OME-NGFF Zarr stores in a Vitessce dataset.

#' Image OME-Zarr object wrapper class
#' @title ImageOmeZarrWrapper Class
#' @docType class
#' @description
#' Class representing an OME-NGFF Zarr store in a Vitessce dataset. Intended to be used with the spatialBeta and layerControllerBeta views.
#'
#' @rdname ImageOmeZarrWrapper
#' @export
ImageOmeZarrWrapper <- R6::R6Class("ImageOmeZarrWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field img_path A local filepath to an OME-NGFF Zarr store.
     #' @keywords internal
     img_path = NULL,
     #' @field img_url A remote URL of an OME-NGFF Zarr store.
     #' @keywords internal
     img_url = NULL,
     #' @field coordinate_transformations A list of coordinate transformations.
     #' @keywords internal
     coordinate_transformations = NULL,
     #' @field coordination_values A list of coordination values.
     #' @keywords internal
     coordination_values = NULL,
     #' @field is_remote Whether or not this image is remote.
     #' @keywords internal
     is_remote = NULL,
     #' @field local_dir_uid
     #' @keywords internal
     local_dir_uid = NULL,
     #' @description
     #' Create a wrapper around an OME-NGFF Zarr store.
     #' @param img_path A local filepath to an OME-NGFF Zarr store.
     #' @param img_url A remote URL of an OME-NGFF Zarr store.
     #' @param coordinate_transformations A list of coordinate transformations.
     #' @param coordination_values A list of coordination values.
     #' @param ... Parameters inherited from `AbstractWrapper`.
     #' @return A new `ImageOmeZarrWrapper` object.
     initialize = function(img_path = NA, img_url = NA, coordinate_transformations = NA, coordination_values = NA, ...) {
       super$initialize(...)
       
       # Check that exactly one of img_path or img_url is provided
       num_inputs <- sum(!is.na(c(img_path, img_url)))
       if(num_inputs != 1) {
         stop("Expected one of img_path or img_url to be provided")
       }

       self$img_path <- img_path
       self$img_url <- img_url
       self$coordinate_transformations <- coordinate_transformations
       self$coordination_values <- coordination_values
       self$is_remote <- !is.na(img_url)

       self$local_dir_uid <- make_unique_filename(".ome.zarr")
     },
     #' @description
     #' Create the web server routes and file definition creators.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @param base_dir A base directory to pass into the convert_and_save function.
     convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
       if(!self$is_remote) {
         super$convert_and_save(dataset_uid, obj_i, base_dir = base_dir)
       }

       # Get the file definition creator functions.
       file_def_creator <- self$make_file_def_creator(dataset_uid, obj_i)
       self$file_def_creators <- append(self$file_def_creators, file_def_creator)

       routes <- self$make_routes(dataset_uid, obj_i)
       self$routes <- c(self$routes, routes)
     },
     #' @description
     #' Create a list representing the server routes.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A list of server route objects.
     #' @keywords internal
     make_routes = function(dataset_uid, obj_i) {
       if(self$is_remote) {
         return(list())
       } else {
         return(self$get_local_dir_route(dataset_uid, obj_i, self$img_path, self$local_dir_uid))
       }
     },
     #' @description
     #' Make the file definition creator function for the image data type.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A file definition creator function which takes a `base_url` parameter.
     make_file_def_creator = function(dataset_uid, obj_i) {
       get_image <- function(base_url) {
         options <- obj_list()
         if(!is_na(self$coordinate_transformations)) {
           options[['coordinateTransformations']] <- self$coordinate_transformations
         }

         file_def <- list(
           fileType = "image.ome-zarr",
           url = self$get_img_url(base_url, dataset_uid, obj_i)
         )
         
         if(length(options) > 0) {
           file_def[['options']] <- options
         }
         if(!is_na(self$coordination_values)) {
           file_def[['coordinationValues']] <- self$coordination_values
         }
         return(file_def)
       }
       return(get_image)
     },
     #' @description
     #' Get the URL to the Zarr store.
     #' @param base_url The base URL for the server.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A URL as a string.
     get_img_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(self$is_remote) {
          return(self$img_url)
        }
        return(self$get_local_dir_url(base_url, dataset_uid, obj_i, self$img_path, self$local_dir_uid))
     }
   ),
)

#' Observation Segmentations OME-Zarr object wrapper class
#' @title ObsSegmentationsOmeZarrWrapper Class
#' @docType class
#' @description
#' Class representing an OME-NGFF Zarr store containing observation segmentations in a Vitessce dataset. Intended to be used with the spatialBeta and layerControllerBeta views.
#'
#' @rdname ObsSegmentationsOmeZarrWrapper
#' @export
ObsSegmentationsOmeZarrWrapper <- R6::R6Class("ObsSegmentationsOmeZarrWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field img_path A local filepath to an OME-NGFF Zarr store.
     #' @keywords internal
     img_path = NULL,
     #' @field img_url A remote URL of an OME-NGFF Zarr store.
     #' @keywords internal
     img_url = NULL,
     #' @field coordinate_transformations A list of coordinate transformations.
     #' @keywords internal
     coordinate_transformations = NULL,
     #' @field coordination_values A list of coordination values.
     #' @keywords internal
     coordination_values = NULL,
     #' @field obs_types_from_channel_names Whether to use the channel names to determine the obs types.
     #' @keywords internal
     obs_types_from_channel_names = NULL,
     #' @field is_remote Whether or not this image is remote.
     #' @keywords internal
     is_remote = NULL,
     #' @field local_dir_uid
     #' @keywords internal
     local_dir_uid = NULL,
     #' @description
     #' Create a wrapper around an OME-NGFF Zarr segmentation store.
     #' @param img_path A local filepath to an OME-NGFF Zarr store.
     #' @param img_url A remote URL of an OME-NGFF Zarr store.
     #' @param coordinate_transformations A list of coordinate transformations.
     #' @param coordination_values A list of coordination values.
     #' @param obs_types_from_channel_names Whether to use the channel names to determine the obs types.
     #' @param ... Parameters inherited from `AbstractWrapper`.
     #' @return A new `ObsSegmentationsOmeZarrWrapper` object.
     initialize = function(img_path = NA, img_url = NA, coordinate_transformations = NA, coordination_values = NA, obs_types_from_channel_names = NA, ...) {
       super$initialize(...)
       
       # Check that exactly one of img_path or img_url is provided
       num_inputs <- sum(!is.na(c(img_path, img_url)))
       if(num_inputs != 1) {
         stop("Expected one of img_path or img_url to be provided")
       }

       self$img_path <- img_path
       self$img_url <- img_url
       self$coordinate_transformations <- coordinate_transformations
       self$obs_types_from_channel_names <- obs_types_from_channel_names
       self$coordination_values <- coordination_values
       self$is_remote <- !is.na(img_url)

       self$local_dir_uid <- make_unique_filename(".ome.zarr")
     },
     #' @description
     #' Create the web server routes and file definition creators.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @param base_dir A base directory to pass into the convert_and_save function.
     convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
       if(!self$is_remote) {
         super$convert_and_save(dataset_uid, obj_i, base_dir = base_dir)
       }

       # Get the file definition creator functions.
       file_def_creator <- self$make_file_def_creator(dataset_uid, obj_i)
       self$file_def_creators <- append(self$file_def_creators, file_def_creator)

       routes <- self$make_routes(dataset_uid, obj_i)
       self$routes <- c(self$routes, routes)
     },
     #' @description
     #' Create a list representing the server routes.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A list of server route objects.
     #' @keywords internal
     make_routes = function(dataset_uid, obj_i) {
       if(self$is_remote) {
         return(list())
       } else {
         return(self$get_local_dir_route(dataset_uid, obj_i, self$img_path, self$local_dir_uid))
       }
     },
     #' @description
     #' Make the file definition creator function for the obs segmentations data type.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A file definition creator function which takes a `base_url` parameter.
     make_file_def_creator = function(dataset_uid, obj_i) {
       get_obs_segmentations <- function(base_url) {
         options <- obj_list()
         if(!is_na(self$coordinate_transformations)) {
           options[['coordinateTransformations']] <- self$coordinate_transformations
         }
         
         if(!is_na(self$obs_types_from_channel_names)) {
           options[['obsTypesFromChannelNames']] <- self$obs_types_from_channel_names
         }

         file_def <- list(
           fileType = "obsSegmentations.ome-zarr",
           url = self$get_img_url(base_url, dataset_uid, obj_i)
         )
         
         if(length(options) > 0) {
           file_def[['options']] <- options
         }
         if(!is_na(self$coordination_values)) {
           file_def[['coordinationValues']] <- self$coordination_values
         }
         return(file_def)
       }
       return(get_obs_segmentations)
     },
     #' @description
     #' Get the URL to the Zarr store.
     #' @param base_url The base URL for the server.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A URL as a string.
     get_img_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(self$is_remote) {
          return(self$img_url)
        }
        return(self$get_local_dir_url(base_url, dataset_uid, obj_i, self$img_path, self$local_dir_uid))
     }
   ),
)