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
     convert_and_save = function(dataset_uid, obj_i) {
       for(image in self$image_wrappers) {
         image$convert_and_save(dataset_uid, obj_i)
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

       if(!is.na(img_url) && !is.na(img_path)) {
         warning("Expected either img_path or img_url to be provided, but not both.")
       }
       self$is_remote <- !is.na(img_url)

     },
     #' @description
     #' Create the web server routes and file definition creators.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     convert_and_save = function(dataset_uid, obj_i) {
       if(!self$is_remote) {
         super$convert_and_save(dataset_uid, obj_i)
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
         route <- VitessceConfigServerRangeRoute$new(
           self$get_route_str(dataset_uid, obj_i, basename(self$img_path)),
           self$img_path
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
       img_url <- NA
       if(self$is_remote) {
         img_url <- self$img_url
       } else {
         img_url <- self$get_url(base_url, dataset_uid, obj_i, basename(self$img_path))
       }

       img_def <- list(
         name = self$name,
         type = "ome-tiff",
         url = img_url
       )
       metadata <- obj_list()
       if(!is.na(self$transformation_matrix)) {
         metadata[['transform']] = list(
           matrix = self$transformation_matrix
         )
       }
       metadata[['isBitmask']] = self$is_bitmask
       img_def[['metadata']] = metadata
       img_def
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
