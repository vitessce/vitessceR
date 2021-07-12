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
    #' @field is_remote Is the data object fully remote?
    #' @keywords internal
    is_remote = NULL,
    #' @field routes A list of `VitessceConfigServerRoute` objects.
    #' @keywords internal
    routes = NULL,
    #' @field file_def_creators A list of file definition creator functions.
    #' @keywords internal
    file_def_creators = NULL,
    #' @description
    #' Create an abstract wrapper around a data object.
    #' @param out_dir The directory for processed output files.
    #' @return A new `AbstractWrapper` object.
    initialize = function(out_dir = NA) {
      if(!is.na(out_dir)) {
        self$out_dir <- out_dir
      } else {
        self$out_dir <- tempdir()
      }
      self$is_remote <- FALSE
      self$routes <- list()
      self$file_def_creators <- list()
    },
    #' @description
    #' Fill in the file_def_creators array.
    #' Each function added to this list should take in a base URL and generate a Vitessce file definition.
    #' If this wrapper is wrapping local data, then create routes and fill in the routes array.
    #' This method is void, should not return anything.
    #'
    #' @param dataset_uid A unique identifier for this dataset.
    #' @param obj_i Within the dataset, the index of this data wrapper object.
    convert_and_save = function(dataset_uid, obj_i) {
      dir.create(self$get_out_dir(dataset_uid, obj_i), recursive = TRUE, showWarnings = FALSE)
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
    #' @description
    #' Create a local web server URL for a dataset object.
    #' @param base_url The base URL on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A URL as a string.
    get_url = function(base_url, dataset_uid, obj_i, ...) {
        retval <- paste0(base_url, self$get_route_str(dataset_uid, obj_i, ...))
        return(retval)
    },
    #' @description
    #' Create a string representing a web server route path (the part following the base URL).
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A path as a string.
    get_route_str = function(dataset_uid, obj_i, ...) {
      retval <- paste0("/", paste(dataset_uid, obj_i, ..., sep = "/"))
      return(retval)
    },
    #' @description
    #' Create a directory path to a dataset within the base output directory.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
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
    #' @field cell_set_meta_names The keys in the Seurat object's meta.data
    #' to use for creating cell sets.
    #' @keywords internal
    cell_set_meta_names = NULL,
    #' @field cell_set_meta_name_mappings The keys in the Seurat object's meta.data
    #' to use for cell set names mapped to new names.
    #' @keywords internal
    cell_set_meta_name_mappings = NULL,
    #' @field cell_set_meta_score_mappings The keys in the Seurat object's meta.data
    #' to use for cell set names mapped to keys for scores.
    #' @keywords internal
    cell_set_meta_score_mappings = NULL,
    #' @field num_genes Number of genes to use for the heatmap.
    #' @keywords internal
    num_genes = NULL,
    #' @description
    #' Create a wrapper around a Seurat object.
    #' @param obj The object to wrap.
    #' @param assay The assay name under the assays part of the Seurat object.
    #' @param cell_set_meta_names An optional list of keys in the object's meta.data
    #' list to use for creating cell sets.
    #' @param cell_set_meta_score_mappings If cell_set_meta_names is provided, this list can
    #' also be provided to map between meta.data keys for set annotations
    #' and keys for annotation scores.
    #' @param cell_set_meta_name_mappings If cell_set_meta_names is provided, this list can
    #' also be provided to map between meta.data keys and new names to replace
    #' the keys in the interface.
    #' @param num_genes The number of genes to include in the expression matrix. Note: this parameter is temporary and should be changed to something more useful such as number of most variable genes.
    #' @param ... Parameters inherited from `AbstractWrapper`.
    #' @return A new `SeuratWrapper` object.
    initialize = function(obj, assay = "RNA", cell_set_meta_names = NA, cell_set_meta_score_mappings = NA, cell_set_meta_name_mappings = NA, num_genes = 10, ...) {
      super$initialize(...)
      self$obj <- obj
      self$assay <- assay
      self$cell_set_meta_names <- cell_set_meta_names
      self$cell_set_meta_score_mappings <- cell_set_meta_score_mappings
      self$cell_set_meta_name_mappings <- cell_set_meta_name_mappings
      self$num_genes <- num_genes
    },
    #' @description
    #' Create the JSON output files, web server routes, and file definition creators.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    convert_and_save = function(dataset_uid, obj_i) {
      super$convert_and_save(dataset_uid, obj_i)

      # Get list representations of the data.
      cells_list <- self$create_cells_list()
      cell_sets_list <- self$create_cell_sets_list()
      expression_matrix_list <- self$create_expression_matrix_list()

      # Convert the lists to JSON strings.
      cells_json <- jsonlite::toJSON(cells_list)
      cell_sets_json <- jsonlite::toJSON(cell_sets_list)
      expression_matrix_json <- jsonlite::toJSON(expression_matrix_list)

      # Save the JSON strings to JSON files.
      write(cells_json, file = self$get_out_dir(dataset_uid, obj_i, "cells.json"))
      write(cell_sets_json, file = self$get_out_dir(dataset_uid, obj_i, "cell-sets.json"))
      write(expression_matrix_json, file = self$get_out_dir(dataset_uid, obj_i, "expression-matrix.json"))


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
    #' Create a list representing the cells in the Seurat object.
    #' @return A list that can be converted to JSON.
    #' @keywords internal
    create_cells_list = function() {
        obj <- self$obj
        embeddings <- slot(obj, "reductions")
        available_embeddings <- names(embeddings)

        cell_ids <- names(slot(obj, "active.ident"))
        cells_list <- obj_list()
        for(cell_id in cell_ids) {
            cells_list[[cell_id]] <- list(
                mappings = obj_list()
            )
        }
        for(embedding_name in available_embeddings) {
            embedding <- embeddings[[embedding_name]]
            embedding_matrix <- slot(embedding, "cell.embeddings")
            for(cell_id in cell_ids) {
                cells_list[[cell_id]]$mappings[[embedding_name]] <- unname(embedding_matrix[cell_id, 1:2])
            }
        }
        cells_list
    },
    #' @description
    #' Create a list representing the cluster assignments in the Seurat object.
    #' @return A list that can be converted to JSON.
    #' @keywords internal
    create_cell_sets_list = function() {
      obj <- self$obj

      meta.data <- slot(obj, "meta.data")
      cells <- Seurat::Idents(obj)

      # https://s3.amazonaws.com/vitessce-data/0.0.31/master_release/linnarsson/linnarsson.cell-sets.json
      cell_sets_list <- list(
        datatype = jsonlite::unbox("cell"),
        version = jsonlite::unbox("0.1.3"),
        tree = list()
      )

      if(!is.na(self$cell_set_meta_names)) {
        for(cell_set_meta_name in self$cell_set_meta_names) {
          cell_set_meta_name_mapped <- cell_set_meta_name
          if(!is.na(self$cell_set_meta_name_mappings) && !is.null(self$cell_set_meta_name_mappings[[cell_set_meta_name]])) {
            cell_set_meta_name_mapped <- self$cell_set_meta_name_mappings[[cell_set_meta_name]]
          }

          cell_set_meta_node <- list(
            name = jsonlite::unbox(cell_set_meta_name_mapped),
            children = list()
          )
          cell_set_annotations <- meta.data[[cell_set_meta_name]]
          cell_set_annotation_scores <- NA
          if(!is.na(self$cell_set_meta_score_mappings) && !is.null(self$cell_set_meta_score_mappings[[cell_set_meta_name]])) {
            cell_set_annotation_scores <- meta.data[[self$cell_set_meta_score_mappings[[cell_set_meta_name]]]]
          }

          cluster_names <- sort(unique(cell_set_annotations))

          for(cluster_name in cluster_names) {
            cells_in_cluster <- names(cells[cell_set_annotations == cluster_name])

            # TODO: find out if there is a way to return NULL
            make_null_tuples <- function(x) { list(jsonlite::unbox(x), jsonlite::unbox(NA)) }
            cells_in_cluster_with_score <- purrr::map(cells_in_cluster, make_null_tuples)
            if(!is.na(cell_set_annotation_scores)) {
              # Scores are available
              score_per_cell <- cell_set_annotation_scores[cell_set_annotations == cluster_name]
              for(i in 1:length(cells_in_cluster)) {
                cells_in_cluster_with_score[[i]][[2]] <- jsonlite::unbox(score_per_cell[[i]])
              }
            }
            cluster_node <- list(
              name = jsonlite::unbox(cluster_name),
              set = cells_in_cluster_with_score
            )
            cell_set_meta_node$children <- append(cell_set_meta_node$children, list(cluster_node))
          }
          cell_sets_list$tree <- append(cell_sets_list$tree, list(cell_set_meta_node))
        }
      }
      cell_sets_list
    },
    #' @description
    #' Create a list representing the cluster assignments in the Seurat object.
    #' @return A list that can be converted to JSON.
    #' @keywords internal
    create_expression_matrix_list = function() {
      # Link to an example clusters.json expression matrix: https://s3.amazonaws.com/vitessce-data/0.0.31/master_release/linnarsson/linnarsson.clusters.json

      result <- list()

      dimnames <- slot(slot(slot(self$obj, "assays")[[self$assay]], "counts"), "Dimnames")
      gene_ids <- dimnames[[1]]
      cell_ids <- dimnames[[2]]

      # TODO: add an option to restrict to highly variable genes or some other subset.
      num_cells <- length(cell_ids)
      #num_genes <- length(gene_ids)
      num_genes <- self$num_genes

      result$rows <- gene_ids[1:num_genes]
      result$cols <- cell_ids

      sparse_matrix <- slot(slot(self$obj, "assays")[[self$assay]], "counts")
      dense_matrix <- as.matrix(sparse_matrix)

      result$matrix <- dense_matrix[1:num_genes, 1:num_cells]
      result$matrix <- result$matrix / max(result$matrix)

      result
    },
    #' @description
    #' Make the file definition creator function for the cells data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A file definition creator function which takes a `base_url` parameter.
    make_cells_file_def_creator = function(dataset_uid, obj_i) {
      get_cells <- function(base_url) {
        file_def <- list(
          type = DataType$CELLS,
          fileType = FileType$CELLS_JSON,
          url = super$get_url(base_url, dataset_uid, obj_i, "cells.json")
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
        file_def <- list(
          type = DataType$CELL_SETS,
          fileType = FileType$CELL_SETS_JSON,
          url = super$get_url(base_url, dataset_uid, obj_i, "cell-sets.json")
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
        file_def <- list(
          type = DataType$EXPRESSION_MATRIX,
          fileType = FileType$CLUSTERS_JSON,
          url = super$get_url(base_url, dataset_uid, obj_i, "expression-matrix.json")
        )
        return(file_def)
      }
      return(get_expression_matrix)
    }
  ),
)

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
