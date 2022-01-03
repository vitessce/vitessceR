#' Seurat object wrapper class
#' @title SeuratWrapper Class
#' @docType class
#' @description
#' Class representing a local Seurat object in a Vitessce dataset.
#'
#' @rdname SeuratWrapper
#' @export
SeuratWrapper <- R6::R6Class("SeuratWrapper",
  inherit = AbstractWrapper,
  public = list(
    #' @field obj The object to wrap.
    #' @keywords internal
    obj = NULL,
    #' @field assay The assay name in the Seurat object.
    #' @keywords internal
    assay = NULL,
    #' @field cell_embedding_names The keys in the Seurat object's reductions/cell.embeddings
    #' to use for creating dimensionality reduction mappings.
    #' @keywords internal
    cell_embeddings = NULL,
    cell_embedding_names = NULL,
    cell_embedding_dims = NULL,
    #' @field cell_set_metas The keys in the Seurat object's meta.data
    #' to use for creating cell sets.
    #' @keywords internal
    cell_set_metas = NULL,
    #' @field cell_set_meta_names The keys in the Seurat object's meta.data
    #' to use for cell set names mapped to new names.
    #' @keywords internal
    cell_set_meta_names = NULL,
    #' @field cell_set_meta_scores The keys in the Seurat object's meta.data
    #' to use for cell set names mapped to keys for scores.
    #' @keywords internal
    cell_set_meta_scores = NULL,
    #' @field zarr_folder
    #' @keywords internal
    zarr_folder = NULL,
    #' @description
    #' Create a wrapper around a Seurat object.
    #' @param obj The object to wrap.
    #' @param assay The assay name under the assays part of the Seurat object.
    #' @param cell_set_metas An optional list of keys in the object's meta.data
    #' list to use for creating cell sets.
    #' @param cell_set_meta_names If cell_set_metas is provided, this list can
    #' also be provided to set new names to replace
    #' the keys in the interface.
    #' @param cell_set_meta_scores If cell_set_metas is provided, this list can
    #' also be provided to map between meta.data keys for set annotations
    #' and keys for annotation scores.
    #' @param ... Parameters inherited from `AbstractWrapper`.
    #' @return A new `SeuratWrapper` object.
    initialize = function(obj, assay = NA, cell_embeddings = NA, cell_embedding_names = NA, cell_embedding_dims = NA, cell_set_metas = NA, cell_set_meta_names = NA, cell_set_meta_scores = NA, ...) {
      super$initialize(...)
      self$obj <- obj
      if(is.na(assay)) {
        self$assay <- "RNA"
      } else {
        self$assay <- assay
      }
      self$cell_embeddings <- cell_embeddings
      self$cell_embedding_names <- cell_embedding_names
      self$cell_embedding_dims <- cell_embedding_dims
      self$cell_set_metas <- cell_set_metas
      self$cell_set_meta_names <- cell_set_meta_names
      self$cell_set_meta_scores <- cell_set_meta_scores

      self$zarr_folder <- "seurat.zarr"
    },
    get_zarr_path = function(dataset_uid, obj_i) {
      out_dir <- super$get_out_dir(dataset_uid, obj_i)
      zarr_filepath <- file.path(out_dir, self$zarr_folder)
      return(zarr_filepath)
    },
    get_zarr_url = function(base_url, dataset_uid, obj_i) {
      return(super$get_url(base_url, dataset_uid, obj_i, self$zarr_folder))
    },
    #' @description
    #' Create the JSON output files, web server routes, and file definition creators.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    convert_and_save = function(dataset_uid, obj_i) {
      super$convert_and_save(dataset_uid, obj_i)

      zarr_filepath <- self$get_zarr_path(dataset_uid, obj_i)
      if(!file.exists(zarr_filepath) || self$overwrite) {
        seurat_to_anndata_zarr(self$obj, out_path = zarr_filepath, assay = self$assay)
      }

      # Get the file definition creator functions.
      cells_file_creator <- self$make_cells_file_def_creator(dataset_uid, obj_i)
      cell_sets_file_creator <- self$make_cell_sets_file_def_creator(dataset_uid, obj_i)
      expression_matrix_file_creator <- self$make_expression_matrix_file_def_creator(dataset_uid, obj_i)

      # Append the new file definition creators functions to the main list.
      self$file_def_creators <- append(self$file_def_creators, cells_file_creator)
      self$file_def_creators <- append(self$file_def_creators, cell_sets_file_creator)
      self$file_def_creators <- append(self$file_def_creators, expression_matrix_file_creator)

      # Create a web server route object for the directory of JSON files.
      self$routes <- append(self$routes, self$get_out_dir_route(dataset_uid, obj_i))
    },
    #' @description
    #' Make the file definition creator function for the cells data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_cells_file_def_creator = function(dataset_uid, obj_i) {
      get_cells <- function(base_url) {
        options <- obj_list()
        if(!is_na(self$cell_embeddings)) {
          options[['mappings']] <- obj_list()
          for(i in 1:length(self$cell_embeddings)) {
            embedding_key <- self$cell_embeddings[i]
            if(!is_na(self$cell_embedding_names)) {
              embedding_name <- self$cell_embedding_names[i]
            } else {
              embedding_name <- embedding_key
            }
            if(!is_na(self$cell_embedding_dims)) {
              embedding_dims <- self$cell_embedding_dims[i]
            } else {
              embedding_dims <- c(0, 1)
            }
            options[['mappings']][[embedding_name]] <- obj_list(
              key = paste0("obsm/X_", embedding_key),
              dims = embedding_dims
            )
          }
        }
        file_def <- list(
          type = DataType$CELLS,
          fileType = FileType$ANNDATA_CELLS_ZARR,
          url = self$get_zarr_url(base_url, dataset_uid, obj_i),
          options = options
        )
        return(file_def)
      }
      return(get_cells)
    },
    #' @description
    #' Make the file definition creator function for the cell sets data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_cell_sets_file_def_creator = function(dataset_uid, obj_i) {
      get_cell_sets <- function(base_url) {
        options <- list()
        if(!is_na(self$cell_set_metas)) {
          for(i in 1:length(self$cell_set_metas)) {
            cell_set_key <- self$cell_set_metas[i]
            if(!is_na(self$cell_set_meta_names)) {
              group_name <- self$cell_set_meta_names[i]
            } else {
              group_name <- cell_set_key
            }

            cell_set_def <- obj_list(
              groupName = group_name,
              setName = paste0("obs/", cell_set_key)
            )
            if(!is_na(self$cell_set_meta_scores)) {
              score_name <- self$cell_set_meta_scores[i]
              # TODO: uncomment
              #cell_set_def[['scoreName']] <- score_name
            }
            options <- append(options, list(cell_set_def))
          }
        }
        file_def <- list(
          type = DataType$CELL_SETS,
          fileType = FileType$ANNDATA_CELL_SETS_ZARR,
          url = self$get_zarr_url(base_url, dataset_uid, obj_i),
          options = options
        )
        return(file_def)
      }
      return(get_cell_sets)
    },
    #' @description
    #' Make the file definition creator function for the expression matrix data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_expression_matrix_file_def_creator = function(dataset_uid, obj_i) {
      get_expression_matrix <- function(base_url) {
        options = obj_list(
          matrix = "X"
        )
        file_def <- list(
          type = DataType$EXPRESSION_MATRIX,
          fileType = FileType$ANNDATA_EXPRESSION_MATRIX_ZARR,
          url = self$get_zarr_url(base_url, dataset_uid, obj_i),
          options = options
        )
        return(file_def)
      }
      return(get_expression_matrix)
    }
  ),
)
