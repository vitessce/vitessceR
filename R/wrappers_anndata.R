#' Seurat object wrapper class
#' @title SeuratWrapper Class
#' @docType class
#' @description
#' Class representing a local Seurat object in a Vitessce dataset.
#'
#' @rdname SeuratWrapper
#' @export
#' @examples
#' obj <- get_seurat_obj()
#' w <- SeuratWrapper$new(
#'   obj,
#'   cell_embeddings = c("pca"),
#'   cell_embedding_names = c("PCA")
#' )
AnnDataWrapper <- R6::R6Class("AnnDataWrapper",
  inherit = AbstractWrapper,
  public = list(
    #' @field adata_path The object to wrap.
    #' @keywords internal
    adata_path = NULL,
    #' @field adata_url The object to wrap.
    #' @keywords internal
    adata_url = NULL,
    #' @field local_dir_uid The path to the local zarr store.
    #' @keywords internal
    local_dir_uid = NULL,


    obs_feature_matrix_path=NULL,
    feature_filter_path=NULL,
    initial_feature_filter_path=NULL,
    obs_set_paths=NULL,
    obs_set_names=NULL,
    obs_locations_path=NULL,
    obs_segmentations_path=NULL,
    obs_embedding_paths=NULL,
    obs_embedding_names=NULL,
    obs_embedding_dims=NULL,
    request_init=NULL,
    feature_labels_path=NULL,
    obs_labels_path=NULL,
    obs_labels_paths=NULL,
    obs_labels_names=NULL,
    coordination_values=NULL,

    #' @description
    #' Create a wrapper around an AnnData object saved to a Zarr store.
    #' @param adata_path The path to a local Zarr store.
    #' @param ... Parameters inherited from `AbstractWrapper`.
    #' @return A new `SeuratWrapper` object.
    initialize = function(adata_path = NA, adata_url = NA, obs_feature_matrix_path = NA, feature_filter_path = NA, initial_feature_filter_path = NA, obs_set_paths = NA, obs_set_names = NA, obs_locations_path = NA, obs_segmentations_path = NA, obs_embedding_paths = NA, obs_embedding_names = NA, obs_embedding_dims = NA, request_init = NA, feature_labels_path = NA, obs_labels_path = NA, coordination_values = NA, obs_labels_paths = NA, obs_labels_names = NA, ...) {
      super$initialize(...)
      self$adata_path <- adata_path
      self$adata_url <- adata_url

      if(!is.na(adata_url) && !is.na(adata_path)) {
        stop("Did not expect adata_url to be provided with adata_path")
      }

      if(is.na(adata_url) && is.na(adata_path)) {
        stop("Expected either adata_url or adata_path to be provided")
      }

      if(!is.na(adata_path)) {
        self$is_remote <- FALSE
      } else {
        self$is_remote <- TRUE
      }

      self$local_dir_uid <- make_unique_filename(".adata.zarr")

      self$obs_feature_matrix_path <- obs_feature_matrix_path
      self$obs_set_names <- obs_set_names
      self$obs_embedding_names <- obs_embedding_names
      self$feature_filter_path <- feature_filter_path
      self$initial_feature_filter_path <- initial_feature_filter_path
      self$obs_set_paths <- obs_set_paths
      self$obs_locations_path <- obs_locations_path
      self$obs_segmentations_path <- obs_segmentations_path
      self$obs_embedding_paths <- obs_embedding_paths
      self$obs_embedding_dims <- obs_embedding_dims
      self$request_init <- request_init
      self$feature_labels_path <- feature_labels_path
      self$obs_labels_paths <- obs_labels_paths
      self$obs_labels_names <- obs_labels_names

      self$coordination_values <- coordination_values
    },
    #' @description
    #' Create the JSON output files, web server routes, and file definition creators.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
      if(self$is_remote) {
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
    make_routes = function(dataset_uid, obj_i) {
      return(self$get_local_dir_route(dataset_uid, obj_i, self$adata_path, self$local_dir_uid))
    },
    #' @description
    #' Get the URL to the Zarr store, to fill in the file URL in the file definitions.
    #' @param base_url The base URL, on which the route will be served.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @keywords internal
    #' @return A URL as a string.
    get_zarr_url = function(base_url, dataset_uid, obj_i) {
      if(self$is_remote) {
        return(self$adata_url)
      } else {
        return(self$get_local_dir_url(base_url, dataset_uid, obj_i, self$adata_path, self$local_dir_uid))
      }
    },
    #' @description
    #' Make the file definition creator function for the cells data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_file_def_creator = function(dataset_uid, obj_i) {
      get_anndata_zarr <- function(base_url) {
        options <- obj_list()
        if(!is_na(self$obs_locations_path)) {
          options[['obsLocations']] <- obj_list()
          options[['obsLocations']][['path']] <- self$obs_locations_path
        }
        if(!is_na(self$obs_segmentations_path)) {
          options[['obsSegmentations']] <- obj_list()
          options[['obsSegmentations']][['path']] <- self$obs_segmentations_path
        }
        if(!is_na(self$obs_embedding_paths)) {
          options[['obsEmbedding']] <- list()
          for(i in seq_len(length(self$obs_embedding_paths))) {
            embedding_path <- self$obs_embedding_paths[i]
            if(!is_na(self$obs_embedding_names)) {
              embedding_name <- self$obs_embedding_names[i]
            } else {
              segments <- stringr::str_split(embedding_path, "/")[[1]]
              embedding_name <- segments[-1]
            }
            if(!is_na(self$obs_embedding_dims)) {
              embedding_dims <- self$obs_embedding_dims[i]
            } else {
              embedding_dims <- c(0, 1)
            }
            options[['obsEmbedding']] <- append(options[['obsEmbedding']], list(obj_list(
              path = embedding_path,
              embeddingType = embedding_name,
              dims = embedding_dims
            )))
          }
        }
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
            options[['obsSets']] <- append(options[['obsSets']], list(obj_list(
              path = set_path,
              name = set_name
            )))
          }
        }
        if(!is_na(self$obs_feature_matrix_path)) {
          options[['obsFeatureMatrix']] <- obj_list()
          options[['obsFeatureMatrix']][['path']] <- self$obs_feature_matrix_path
          if(!is_na(self$feature_filter_path)) {
            options[['obsFeatureMatrix']][['featureFilterPath']] <- self$feature_filter_path
          }
          if(!is_na(self$initial_feature_filter_path)) {
            options[['obsFeatureMatrix']][['initialFeatureFilterPath']] <- self$initial_feature_filter_path
          }
        }
        if(!is_na(self$feature_labels_path)) {
          options[['featureLabels']] <- obj_list()
          options[['featureLabels']][['path']] <- self$feature_labels_path
        }
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

        file_def <- list(
          fileType = FileType$ANNDATA_ZARR,
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
      return(get_anndata_zarr)
    }
  ),
)
