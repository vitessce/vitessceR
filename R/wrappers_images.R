#' Image wrapper class
#' @title MultiImageWrapper Class
#' @docType class
#' @description
#' Class representing image objects in a Vitessce dataset.
#'
#' @rdname MultiImageWrapper
#' @export
MultiImageWrapper <- R6::R6Class("MultiImageWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field image_wrappers The object to wrap.
     #' @keywords internal
     image_wrappers = NULL,
     #' @field use_physical_size_scaling Whether or not to scale the image based on the physical size metadata stored in the file.
     #' @keywords internal
     use_physical_size_scaling = NULL,
     #' @description
     #' Create a wrapper around multiple image objects.
     #' @param image_wrappers A list of individual image wrapper objects.
     #' @param use_physical_size_scaling Whether or not to scale the image based on the physical size metadata stored in the file.
     #' @param ... Parameters inherited from `AbstractWrapper`.
     #' @return A new `MultiImageWrapper` object.
     initialize = function(image_wrappers, use_physical_size_scaling = FALSE, ...) {
       super$initialize(...)
       self$image_wrappers <- image_wrappers
       self$use_physical_size_scaling <- use_physical_size_scaling
     },
     #' @description
     #' Create the web server routes and file definition creators.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @param base_dir A base directory to pass into the convert_and_save function of each image wrapper.
     convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
       for(image in self$image_wrappers) {
         image$convert_and_save(dataset_uid, obj_i, base_dir = base_dir)
       }

       # Get the file definition creator functions.
       raster_file_creator <- self$make_raster_file_def_creator(dataset_uid, obj_i)

       # Append the new file definition creators functions to the main list.
       self$file_def_creators <- append(self$file_def_creators, raster_file_creator)

       # Create a web server route object for the directory of JSON files.
       routes <- self$make_raster_routes()
       self$routes <- append(self$routes, routes)
     },
     #' @description
     #' Create a list representing the image routes.
     #' @return A list of server route objects.
     #' @keywords internal
     make_raster_routes = function() {
       obj_routes <- list()
       for(i in length(self$image_wrappers)) {
         image <- self$image_wrappers[[i]]
         image_routes <- image$get_routes()
         obj_routes <- c(obj_routes, image_routes)
       }
       obj_routes
     },
     #' @description
     #' Make the file definition creator function for the raster data type.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A file definition creator function which takes a `base_url` parameter.
     make_raster_file_def_creator = function(dataset_uid, obj_i) {
       get_raster <- function(base_url) {
         options_def <- list(
           schemaVersion = "0.0.2",
           usePhysicalSizeScaling = self$use_physical_size_scaling,
           images = list(),
           renderLayers = list()
         )
         for(image in self$image_wrappers) {
           image_def <- image$make_image_def(dataset_uid, obj_i, base_url)
           options_def$images <- append(options_def$images, list(image_def))
           options_def$renderLayers <- append(options_def$renderLayers, image$name)
         }
         file_def <- list(
           type = DataType$RASTER,
           fileType = FileType$RASTER_JSON,
           options = options_def
         )
         return(file_def)
       }
       return(get_raster)
     }
   ),
)

#' OME-TIFF object wrapper class
#' @title OmeTiffWrapper Class
#' @docType class
#' @description
#' Class representing an OME-TIFF file in a Vitessce dataset.
#'
#' @rdname OmeTiffWrapper
#' @export
OmeTiffWrapper <- R6::R6Class("OmeTiffWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field img_path A local filepath to an OME-TIFF file.
     #' @keywords internal
     img_path = NULL,
     #' @field img_url A remote URL of an OME-TIFF file.
     #' @keywords internal
     img_url = NULL,
     #' @field name The display name for this OME-TIFF within Vitessce.
     #' @keywords internal
     name = NULL,
     #' @field transformation_matrix A column-major ordered matrix for transforming
     #' this image (see http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#homogeneous-coordinates for more information).
     #' @keywords internal
     transformation_matrix = NULL,
     #' @field is_bitmask Whether or not this image is a bitmask.
     #' @keywords internal
     is_bitmask = NULL,
     #' @field is_remote Whether or not this image is remote.
     #' @keywords internal
     is_remote = NULL,
     #' @field local_img_uid
     #' @keywords internal
     local_img_uid = NULL,
     #' @description
     #' Create a wrapper around multiple image objects.
     #' @param img_path A local filepath to an OME-TIFF file.
     #' @param img_url A remote URL of an OME-TIFF file.
     #' @param name The display name for this OME-TIFF within Vitessce.
     #' @param transformation_matrix A column-major ordered matrix for transforming
     #' this image (see http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/#homogeneous-coordinates for more information).
     #' @param is_bitmask Whether or not this image is a bitmask.
     #' @param use_physical_size_scaling Whether or not to scale the image based on the physical size metadata stored in the file.
     #' @param ... Parameters inherited from `AbstractWrapper`.
     #' @return A new `OmeTiffWrapper` object.
     initialize = function(img_path = NA, img_url = NA, name = "", transformation_matrix = NA, is_bitmask = FALSE, ...) {
       super$initialize(...)
       self$img_path <- img_path
       self$img_url <- img_url
       self$name <- name
       self$transformation_matrix <- transformation_matrix
       self$is_bitmask <- is_bitmask

       self$local_img_uid <- make_unique_filename(".ome.tif")

       if(!is.na(img_url) && !is.na(img_path)) {
         warning("Expected either img_path or img_url to be provided, but not both.")
       }
       self$is_remote <- !is.na(img_url)

     },
     #' @description
     #' Create the web server routes and file definition creators.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
       if(!self$is_remote) {
         super$convert_and_save(dataset_uid, obj_i, base_dir = base_dir)
       }

       # Get the file definition creator functions.
       raster_file_creator <- self$make_raster_file_def_creator(dataset_uid, obj_i)
       self$file_def_creators <- append(self$file_def_creators, raster_file_creator)

       routes <- self$make_raster_routes(dataset_uid, obj_i)
       self$routes <- c(self$routes, routes)
     },
     #' @description
     #' Create a list representing the server routes.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A list of server route objects.
     #' @keywords internal
     make_raster_routes = function(dataset_uid, obj_i) {
       if(self$is_remote) {
         return(list())
       } else {
        if(is.na(self$base_dir)) {
          local_img_path <- self$img_path
          local_img_route_path <- self$get_route_str(dataset_uid, obj_i, self$local_img_uid)
        } else {
          local_img_path <- file.path(self$base_dir, self$img_path)
          local_img_route_path <- file_path_to_url_path(self$img_path)
        }
        route <- VitessceConfigServerRangeRoute$new(
          local_img_route_path,
          local_img_path
        )
        return(list(route))
       }
     },
     #' @description
     #' Create an object representing a single image in a raster.json list of images.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @param base_url The base URL for the server.
     #' @return A list that can be converted to JSON.
     #' @keywords internal
     make_image_def = function(dataset_uid, obj_i, base_url) {
      img_url <- self$get_img_url(base_url, dataset_uid, obj_i)
      return(self$create_image_json(img_url))
     },
     #' @description
     #' Make the file definition creator function for the raster data type.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A file definition creator function which takes a `base_url` parameter.
     make_raster_file_def_creator = function(dataset_uid, obj_i) {
       get_raster <- function(base_url) {
         options_def <- list(
           schemaVersion = "0.0.2",
           images = list(
             self$make_image_def(dataset_uid, obj_i, base_url)
           )
         )
         file_def <- list(
           fileType = FileType$RASTER_JSON,
           options = options_def
         )
         return(file_def)
       }
       return(get_raster)
     },
      create_image_json = function(img_url, offsets_url = NA) {
        metadata <- obj_list()
        img_def <- list(
         name = self$name,
         type = "ome-tiff",
         url = img_url
        )
        # TODO: offsets_url
       if(!is.na(self$transformation_matrix)) {
         metadata[['transform']] = list(
           matrix = self$transformation_matrix
         )
       }
       metadata[['isBitmask']] = self$is_bitmask
       img_def[['metadata']] = metadata
       img_def
     },
     get_img_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(self$is_remote) {
          return(self$img_url)
        }
        if(!is.na(self$base_dir)) {
          return(self$get_url_simple(base_url, file_path_to_url_path(self$img_path, prepend_slash = FALSE)))
        }
        return(self$get_url(base_url, dataset_uid, obj_i, self$local_img_uid))
     }
   ),
)

#' Image OME-TIFF object wrapper class
#' @title ImageOmeTiffWrapper Class
#' @docType class
#' @description
#' Class representing an OME-TIFF file in a Vitessce dataset. Intended to be used with the spatialBeta and layerControllerBeta views.
#'
#' @rdname ImageOmeTiffWrapper
#' @export
ImageOmeTiffWrapper <- R6::R6Class("ImageOmeTiffWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field img_path A local filepath to an OME-TIFF file.
     #' @keywords internal
     img_path = NULL,
     #' @field img_url A remote URL of an OME-TIFF file.
     #' @keywords internal
     img_url = NULL,
     #' @field offsets_path A local filepath to an offsets.json file.
     #' @keywords internal
     offsets_path = NULL,
     #' @field offsets_url A remote URL of an offsets.json file.
     #' @keywords internal
     offsets_url = NULL,
     #' @field coordinate_transformations A list of coordinate transformations.
     #' @keywords internal
     coordinate_transformations = NULL,
     #' @field coordination_values A list of coordination values.
     #' @keywords internal
     coordination_values = NULL,
     #' @field is_remote Whether or not this image is remote.
     #' @keywords internal
     is_remote = NULL,
     #' @field local_img_uid
     #' @keywords internal
     local_img_uid = NULL,
     #' @field local_offsets_uid
     #' @keywords internal
     local_offsets_uid = NULL,
     #' @description
     #' Create a wrapper around an OME-TIFF image.
     #' @param img_path A local filepath to an OME-TIFF file.
     #' @param img_url A remote URL of an OME-TIFF file.
     #' @param offsets_path A local filepath to an offsets.json file.
     #' @param offsets_url A remote URL of an offsets.json file.
     #' @param coordinate_transformations A list of coordinate transformations.
     #' @param coordination_values A list of coordination values.
     #' @param ... Parameters inherited from `AbstractWrapper`.
     #' @return A new `ImageOmeTiffWrapper` object.
     initialize = function(img_path = NA, img_url = NA, offsets_path = NA, offsets_url = NA, coordinate_transformations = NA, coordination_values = NA, ...) {
       super$initialize(...)
       
       # Check that exactly one of img_path or img_url is provided
       num_inputs <- sum(!is.na(c(img_path, img_url)))
       if(num_inputs != 1) {
         stop("Expected one of img_path or img_url to be provided")
       }
       
       # Check that at most one of offsets_path or offsets_url is provided
       num_offsets <- sum(!is.na(c(offsets_path, offsets_url)))
       if(num_offsets > 1) {
         stop("Expected zero or one of offsets_path or offsets_url to be provided")
       }

       self$img_path <- img_path
       self$img_url <- img_url
       self$offsets_path <- offsets_path
       self$offsets_url <- offsets_url
       self$coordinate_transformations <- coordinate_transformations
       self$coordination_values <- coordination_values
       self$is_remote <- !is.na(img_url)

       self$local_img_uid <- make_unique_filename(".ome.tif")
       self$local_offsets_uid <- make_unique_filename(".offsets.json")
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
         routes <- list()
         if(is.na(self$base_dir)) {
           local_img_path <- self$img_path
           local_img_route_path <- self$get_route_str(dataset_uid, obj_i, self$local_img_uid)
           local_offsets_route_path <- self$get_route_str(dataset_uid, obj_i, self$local_offsets_uid)
         } else {
           local_img_path <- file.path(self$base_dir, self$img_path)
           local_img_route_path <- file_path_to_url_path(self$img_path)
           # Do not include offsets in base_dir mode
           local_offsets_route_path <- NA
         }
         
         img_route <- VitessceConfigServerRangeRoute$new(
           local_img_route_path,
           local_img_path
         )
         routes <- append(routes, img_route)
         
         # TODO: Add offsets route when not in base_dir mode
         # This would require implementing offsets generation logic
         
         return(routes)
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
         
         offsets_url <- self$get_offsets_url(base_url, dataset_uid, obj_i)
         if(!is.na(offsets_url) && is.na(self$base_dir)) {
           # Do not include offsets in base_dir mode
           options[['offsetsUrl']] <- offsets_url
         }

         file_def <- list(
           fileType = "image.ome-tiff",
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
     #' Get the URL to the image file.
     #' @param base_url The base URL for the server.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A URL as a string.
     get_img_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(self$is_remote) {
          return(self$img_url)
        }
        if(!is.na(self$base_dir)) {
          return(self$get_url_simple(base_url, file_path_to_url_path(self$img_path, prepend_slash = FALSE)))
        }
        return(self$get_url(base_url, dataset_uid, obj_i, self$local_img_uid))
     },
     #' @description
     #' Get the URL to the offsets file.
     #' @param base_url The base URL for the server.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A URL as a string or NA.
     get_offsets_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(!is.na(self$offsets_url) || self$is_remote) {
          return(self$offsets_url)
        }
        if(!is.na(self$offsets_path)) {
          return(self$get_url(base_url, dataset_uid, obj_i, self$local_offsets_uid))
        }
        return(NA)
     }
   ),
)

#' Observation Segmentations OME-TIFF object wrapper class
#' @title ObsSegmentationsOmeTiffWrapper Class
#' @docType class
#' @description
#' Class representing an OME-TIFF file containing observation segmentations in a Vitessce dataset. Intended to be used with the spatialBeta and layerControllerBeta views.
#'
#' @rdname ObsSegmentationsOmeTiffWrapper
#' @export
ObsSegmentationsOmeTiffWrapper <- R6::R6Class("ObsSegmentationsOmeTiffWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field img_path A local filepath to an OME-TIFF file.
     #' @keywords internal
     img_path = NULL,
     #' @field img_url A remote URL of an OME-TIFF file.
     #' @keywords internal
     img_url = NULL,
     #' @field offsets_path A local filepath to an offsets.json file.
     #' @keywords internal
     offsets_path = NULL,
     #' @field offsets_url A remote URL of an offsets.json file.
     #' @keywords internal
     offsets_url = NULL,
     #' @field coordinate_transformations A list of coordinate transformations.
     #' @keywords internal
     coordinate_transformations = NULL,
     #' @field obs_types_from_channel_names Whether to use the channel names to determine the obs types.
     #' @keywords internal
     obs_types_from_channel_names = NULL,
     #' @field coordination_values A list of coordination values.
     #' @keywords internal
     coordination_values = NULL,
     #' @field is_remote Whether or not this image is remote.
     #' @keywords internal
     is_remote = NULL,
     #' @field local_img_uid
     #' @keywords internal
     local_img_uid = NULL,
     #' @field local_offsets_uid
     #' @keywords internal
     local_offsets_uid = NULL,
     #' @description
     #' Create a wrapper around an OME-TIFF segmentation image.
     #' @param img_path A local filepath to an OME-TIFF file.
     #' @param img_url A remote URL of an OME-TIFF file.
     #' @param offsets_path A local filepath to an offsets.json file.
     #' @param offsets_url A remote URL of an offsets.json file.
     #' @param coordinate_transformations A list of coordinate transformations.
     #' @param obs_types_from_channel_names Whether to use the channel names to determine the obs types.
     #' @param coordination_values A list of coordination values.
     #' @param ... Parameters inherited from `AbstractWrapper`.
     #' @return A new `ObsSegmentationsOmeTiffWrapper` object.
     initialize = function(img_path = NA, img_url = NA, offsets_path = NA, offsets_url = NA, coordinate_transformations = NA, obs_types_from_channel_names = NA, coordination_values = NA, ...) {
       super$initialize(...)
       
       # Check that exactly one of img_path or img_url is provided
       num_inputs <- sum(!is.na(c(img_path, img_url)))
       if(num_inputs != 1) {
         stop("Expected one of img_path or img_url to be provided")
       }
       
       # Check that at most one of offsets_path or offsets_url is provided
       num_offsets <- sum(!is.na(c(offsets_path, offsets_url)))
       if(num_offsets > 1) {
         stop("Expected zero or one of offsets_path or offsets_url to be provided")
       }

       self$img_path <- img_path
       self$img_url <- img_url
       self$offsets_path <- offsets_path
       self$offsets_url <- offsets_url
       self$coordinate_transformations <- coordinate_transformations
       self$obs_types_from_channel_names <- obs_types_from_channel_names
       self$coordination_values <- coordination_values
       self$is_remote <- !is.na(img_url)

       self$local_img_uid <- make_unique_filename(".ome.tif")
       self$local_offsets_uid <- make_unique_filename(".offsets.json")
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
         routes <- list()
         if(is.na(self$base_dir)) {
           local_img_path <- self$img_path
           local_img_route_path <- self$get_route_str(dataset_uid, obj_i, self$local_img_uid)
           local_offsets_route_path <- self$get_route_str(dataset_uid, obj_i, self$local_offsets_uid)
         } else {
           local_img_path <- file.path(self$base_dir, self$img_path)
           local_img_route_path <- file_path_to_url_path(self$img_path)
           # Do not include offsets in base_dir mode
           local_offsets_route_path <- NA
         }
         
         img_route <- VitessceConfigServerRangeRoute$new(
           local_img_route_path,
           local_img_path
         )
         routes <- append(routes, img_route)
         
         # TODO: Add offsets route when not in base_dir mode
         # This would require implementing offsets generation logic
         
         return(routes)
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
         
         offsets_url <- self$get_offsets_url(base_url, dataset_uid, obj_i)
         if(!is.na(offsets_url) && is.na(self$base_dir)) {
           # Do not include offsets in base_dir mode
           options[['offsetsUrl']] <- offsets_url
         }

         file_def <- list(
           fileType = "obsSegmentations.ome-tiff",
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
     #' Get the URL to the image file.
     #' @param base_url The base URL for the server.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A URL as a string.
     get_img_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(self$is_remote) {
          return(self$img_url)
        }
        if(!is.na(self$base_dir)) {
          return(self$get_url_simple(base_url, file_path_to_url_path(self$img_path, prepend_slash = FALSE)))
        }
        return(self$get_url(base_url, dataset_uid, obj_i, self$local_img_uid))
     },
     #' @description
     #' Get the URL to the offsets file.
     #' @param base_url The base URL for the server.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A URL as a string or NA.
     get_offsets_url = function(base_url = "", dataset_uid = "", obj_i = "") {
        if(!is.na(self$offsets_url) || self$is_remote) {
          return(self$offsets_url)
        }
        if(!is.na(self$offsets_path)) {
          return(self$get_url(base_url, dataset_uid, obj_i, self$local_offsets_uid))
        }
        return(NA)
     }
   ),
)
