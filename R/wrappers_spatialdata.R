#' SpatialData object wrapper class
#' @title SpatialDataWrapper Class
#' @docType class
#' @description
#' Class representing a SpatialData object in a Vitessce dataset.
#'
#' @rdname SpatialDataWrapper
#' @export
SpatialDataWrapper <- R6::R6Class("SpatialDataWrapper",
  inherit = AnnDataWrapper,
  public = list(
    #' @field sdata_path The path to a local SpatialData Zarr store.
    #' @keywords internal
    sdata_path = NULL,
    #' @field sdata_url The URL to a remote SpatialData Zarr store.
    #' @keywords internal
    sdata_url = NULL,
    #' @field image_path Path to the image element of interest.
    #' @keywords internal
    image_path = NULL,
    #' @field region The region to use.
    #' @keywords internal
    region = NULL,
    #' @field coordinate_system Name of a target coordinate system.
    #' @keywords internal
    coordinate_system = NULL,
    #' @field obs_spots_path Location of shapes that should be interpreted as spot observations.
    #' @keywords internal
    obs_spots_path = NULL,
    #' @field obs_segmentations_path Path to a labels or shapes element.
    #' @keywords internal
    obs_segmentations_path = NULL,
    #' @field table_path The path to the table within the SpatialData store.
    #' @keywords internal
    table_path = NULL,
    #' @field is_zip Boolean indicating whether the Zarr store is in a zipped format.
    #' @keywords internal
    is_zip = NULL,
    #' @description
    #' Create a wrapper around a SpatialData object.
    #' @param sdata_path SpatialData path, exclusive with other sdata_xxxx arguments.
    #' @param sdata_url SpatialData url, exclusive with other sdata_xxxx arguments.
    #' @param image_path Path to the image element of interest.
    #' @param region The region to use.
    #' @param coordinate_system Name of a target coordinate system.
    #' @param obs_spots_path Location of shapes that should be interpreted as spot observations.
    #' @param obs_segmentations_path Path to a labels or shapes element (segmentation bitmask label image or segmentation polygon shapes).
    #' @param table_path The path to the table within the SpatialData store. Default is "tables/table".
    #' @param is_zip Boolean indicating whether the Zarr store is in a zipped format.
    #' @param coordination_values Coordination values for the file definition.
    #' @param ... Parameters inherited from `AnnDataWrapper`.
    #' @return A new `SpatialDataWrapper` object.
    initialize = function(sdata_path = NA, sdata_url = NA, image_path = NA, region = NA, coordinate_system = NA, obs_spots_path = NA, obs_segmentations_path = NA, table_path = "tables/table", is_zip = NA, coordination_values = NA, ...) {
      
      # Check that exactly one of sdata_path or sdata_url is provided
      num_inputs <- sum(!is.na(c(sdata_path, sdata_url)))
      if(num_inputs != 1) {
        stop("Expected one of sdata_path or sdata_url to be provided")
      }
      
      # Initialize parent class with sdata parameters mapped to adata parameters
      super$initialize(adata_path = sdata_path, adata_url = sdata_url, coordination_values = coordination_values, ...)
      
      self$sdata_path <- sdata_path
      self$sdata_url <- sdata_url
      self$image_path <- image_path
      self$region <- region
      self$coordinate_system <- coordinate_system
      self$obs_spots_path <- obs_spots_path
      self$obs_segmentations_path <- obs_segmentations_path
      self$table_path <- table_path
      self$is_zip <- is_zip
      
      # Update the local_dir_uid for SpatialData
      self$local_dir_uid <- make_unique_filename(".sdata.zarr")
      
      # Update zarr_folder if using local path
      if(!is.na(sdata_path)) {
        self$zarr_folder <- 'spatialdata.zarr'
      }
    },
    #' @description
    #' Make the file definition creator function for the SpatialData data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_file_def_creator = function(dataset_uid, obj_i) {
      get_spatialdata_zarr <- function(base_url) {
        options <- obj_list()
        
        # Generate options for SpatialData-specific schema
        options <- self$gen_sdata_obs_locations_schema(options)
        options <- self$gen_sdata_obs_segmentations_schema(options)
        options <- self$gen_sdata_obs_spots_schema(options)
        options <- self$gen_sdata_image_schema(options)
        options <- self$gen_sdata_obs_feature_matrix_schema(options)
        options <- self$gen_sdata_obs_sets_schema(options)
        
        # Add feature labels if specified
        if(!is_na(self$feature_labels_path)) {
          options[['featureLabels']] <- obj_list()
          options[['featureLabels']][['path']] <- self$feature_labels_path
        }
        
        # Add obs labels if specified
        if(!is_na(self$obs_labels_paths)) {
          options[['obsLabels']] <- list()
          for(i in seq_len(length(self$obs_labels_paths))) {
            obs_labels_path <- self$obs_labels_paths[i]
            if(!is_na(self$obs_labels_names)) {
              obs_labels_name <- self$obs_labels_names[i]
            } else {
              segments <- stringr::str_split(obs_labels_path, "/")[[1]]
              obs_labels_name <- segments[-1]
            }
            options[['obsLabels']] <- append(options[['obsLabels']], list(obj_list(
              path = obs_labels_path,
              obsLabelsType = obs_labels_name
            )))
          }
        }

        if(length(options) > 0) {
          file_type <- if(!is.na(self$is_zip) && self$is_zip) "spatialdata.zarr.zip" else "spatialdata.zarr"
          
          file_def <- list(
            fileType = file_type,
            url = self$get_zarr_url(base_url, dataset_uid, obj_i),
            options = options
          )
          
          if(!is_na(self$request_init)) {
            file_def[['requestInit']] <- self$request_init
          }
          if(!is_na(self$coordination_values)) {
            file_def[['coordinationValues']] <- self$coordination_values
          }
          
          return(file_def)
        }
        return(NULL)
      }
      return(get_spatialdata_zarr)
    },
    #' @description
    #' Generate obs locations schema for SpatialData.
    #' @param options The options object to modify.
    #' @return The modified options object.
    gen_sdata_obs_locations_schema = function(options) {
      if(!is_na(self$obs_locations_path)) {
        options[['obsLocations']] <- obj_list()
        options[['obsLocations']][['path']] <- self$obs_locations_path
        if(!is_na(self$table_path)) {
          options[['obsLocations']][['tablePath']] <- self$table_path
        }
        if(!is_na(self$region)) {
          options[['obsLocations']][['region']] <- self$region
        }
        if(!is_na(self$coordinate_system)) {
          options[['obsLocations']][['coordinateSystem']] <- self$coordinate_system
        }
      }
      return(options)
    },
    #' @description
    #' Generate obs segmentations schema for SpatialData.
    #' @param options The options object to modify.
    #' @return The modified options object.
    gen_sdata_obs_segmentations_schema = function(options) {
      if(!is_na(self$obs_segmentations_path)) {
        options[['obsSegmentations']] <- obj_list()
        options[['obsSegmentations']][['path']] <- self$obs_segmentations_path
        if(!is_na(self$table_path)) {
          options[['obsSegmentations']][['tablePath']] <- self$table_path
        }
        if(!is_na(self$coordinate_system)) {
          options[['obsSegmentations']][['coordinateSystem']] <- self$coordinate_system
        }
      }
      return(options)
    },
    #' @description
    #' Generate obs spots schema for SpatialData.
    #' @param options The options object to modify.
    #' @return The modified options object.
    gen_sdata_obs_spots_schema = function(options) {
      if(!is_na(self$obs_spots_path)) {
        options[['obsSpots']] <- obj_list()
        options[['obsSpots']][['path']] <- self$obs_spots_path
        if(!is_na(self$table_path)) {
          options[['obsSpots']][['tablePath']] <- self$table_path
        }
        if(!is_na(self$region)) {
          options[['obsSpots']][['region']] <- self$region
        }
        if(!is_na(self$coordinate_system)) {
          options[['obsSpots']][['coordinateSystem']] <- self$coordinate_system
        }
      }
      return(options)
    },
    #' @description
    #' Generate image schema for SpatialData.
    #' @param options The options object to modify.
    #' @return The modified options object.
    gen_sdata_image_schema = function(options) {
      if(!is_na(self$image_path)) {
        options[['image']] <- obj_list()
        options[['image']][['path']] <- self$image_path
        if(!is_na(self$coordinate_system)) {
          options[['image']][['coordinateSystem']] <- self$coordinate_system
        }
      }
      return(options)
    },
    #' @description
    #' Generate obs feature matrix schema for SpatialData.
    #' @param options The options object to modify.
    #' @return The modified options object.
    gen_sdata_obs_feature_matrix_schema = function(options) {
      if(!is_na(self$obs_feature_matrix_path)) {
        options[['obsFeatureMatrix']] <- obj_list()
        options[['obsFeatureMatrix']][['path']] <- self$obs_feature_matrix_path
        if(!is_na(self$feature_filter_path)) {
          options[['obsFeatureMatrix']][['featureFilterPath']] <- self$feature_filter_path
        }
        if(!is_na(self$initial_feature_filter_path)) {
          options[['obsFeatureMatrix']][['initialFeatureFilterPath']] <- self$initial_feature_filter_path
        }
        if(!is_na(self$region)) {
          options[['obsFeatureMatrix']][['region']] <- self$region
        }
      }
      return(options)
    },
    #' @description
    #' Generate obs sets schema for SpatialData.
    #' @param options The options object to modify.
    #' @return The modified options object.
    gen_sdata_obs_sets_schema = function(options) {
      if(!is_na(self$obs_set_paths)) {
        options[['obsSets']] <- list()
        for(i in seq_len(length(self$obs_set_paths))) {
          set_path <- self$obs_set_paths[i]
          if(!is_na(self$obs_set_names)) {
            set_name <- self$obs_set_names[i]
          } else {
            segments <- stringr::str_split(set_path, "/")[[1]]
            set_name <- segments[-1]
          }
          set_obj <- obj_list(
            path = set_path,
            name = set_name
          )
          if(!is_na(self$table_path)) {
            set_obj[['tablePath']] <- self$table_path
          }
          if(!is_na(self$region)) {
            set_obj[['region']] <- self$region
          }
          options[['obsSets']] <- append(options[['obsSets']], list(set_obj))
        }
      }
      return(options)
    }
  ),
)