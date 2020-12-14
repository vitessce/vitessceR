#' Abstract dataset object wrapper class
#' @title AbstractWrapper Class
#' @docType class
#' @description
#' Abstract class representing a local dataset object in a Vitessce dataset.
#'
#' @rdname AbstractWrapper
#' @export
#' @keywords internal
AbstractWrapper <- R6::R6Class("AbstractWrapper",
  public = list(
    #' @description
    #' Create a response function.
    #' @param data_list The data to serve, as a list. Will be converted to JSON.
    #' @return A new response function.
    create_response_json = function(data_list) {
        response_func <- function(req, res) {
            data_list
        }
        response_func
    },
    #' @description
    #' Get the routes and file definitions for the cells data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_cells = function(port, dataset_uid, obj_i) {
        NA
    },
    #' @description
    #' Get the routes and file definitions for the cell sets data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_cell_sets = function(port, dataset_uid, obj_i) {
        NA
    },
    #' @description
    #' Get the routes and file definitions for the raster type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_raster = function(port, dataset_uid, obj_i) {
        NA
    },
    #' @description
    #' Get the routes and file definitions for the molecules data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_molecules = function(port, dataset_uid, obj_i) {
        NA
    },
    #' @description
    #' Get the routes and file definitions for the neighborhoods data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_neighborhoods = function(port, dataset_uid, obj_i) {
        NA
    },
    #' @description
    #' Get the routes and file definitions for the expression matrix data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_expression_matrix = function(port, dataset_uid, obj_i) {
        NA
    },
    #' @description
    #' Get the routes and file definitions for a data type.
    #' @param data_type A data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return NA or a list of `routes` and `file_defs` lists.
    get_data = function(data_type, port, dataset_uid, obj_i) {
        if(data_type == DataType$CELLS) {
            retval <- self$get_cells(port, dataset_uid, obj_i)
        } else if(data_type == DataType$CELL_SETS) {
            retval <- self$get_cell_sets(port, dataset_uid, obj_i)
        } else if(data_type == DataType$RASTER) {
            retval <- self$get_raster(port, dataset_uid, obj_i)
        } else if(data_type == DataType$MOLECULES) {
            retval <- self$get_molecules(port, dataset_uid, obj_i)
        } else if(data_type == DataType$NEIGHBORHOODS) {
            retval <- self$get_neighborhoods(port, dataset_uid, obj_i)
        } else if(data_type == DataType$EXPRESSION_MATRIX) {
            retval <- self$get_expression_matrix(port, dataset_uid, obj_i)
        }
        retval
    },
    #' @description
    #' Create a local web server URL for a dataset object data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @param suffix A suffix for the URL.
    #' @return A URL as a string.
    get_url = function(port, dataset_uid, obj_i, suffix) {
        retval <- paste("http://localhost:", port, "/", dataset_uid, "/", obj_i, "/", suffix, sep="")
    },
    #' @description
    #' Create a web server route path for a dataset object data type.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @param suffix A suffix for the URL.
    #' @return A path as a string.
    get_route = function(dataset_uid, obj_i, suffix) {
        retval <- paste("/", dataset_uid, "/", obj_i, "/", suffix, sep="")
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
    #' @description
    #' Create a wrapper around a Seurat object.
    #' @param obj The object to wrap.
    #' @return A new `SeuratWrapper` object.
    initialize = function(obj) {
      self$obj <- obj
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
      cells <- Seurat::Idents(obj)
      cluster_names <- levels(cells)

      cell_sets_list <- list(
        datatype = jsonlite::unbox("cell"),
        version = jsonlite::unbox("0.1.2"),
        tree = list(
          list(
            name = jsonlite::unbox("Clusters"),
            children = list()
          )
        )
      )

      for(cluster_name in cluster_names) {
        cells_in_cluster <- names(cells[cells == cluster_name])
        cluster_node <- list(
          name = jsonlite::unbox(paste("Cluster", cluster_name)),
          set = cells_in_cluster
        )
        cell_sets_list$tree[[1]]$children <- append(cell_sets_list$tree[[1]]$children, list(cluster_node))
      }
      cell_sets_list
    },
    #' @description
    #' Get the routes and file definitions for the cells data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A list of `routes` and `file_defs` lists.
    get_cells = function(port, dataset_uid, obj_i) {
        retval <- list(
            routes = list(),
            file_defs = list()
        )

        cells_list <- self$create_cells_list()

        retval$routes <- list(
            VitessceConfigServerRoute$new(
                super$get_route(dataset_uid, obj_i, "cells"),
                super$create_response_json(cells_list)
            )
        )
        retval$file_defs <- list(
            list(
                type = DataType$CELLS,
                fileType = FileType$CELLS_JSON,
                url = super$get_url(port, dataset_uid, obj_i, "cells")
            )
        )
        retval
    },
    #' @description
    #' Get the routes and file definitions for the cell sets data type.
    #' @param port The port on which the web server is serving.
    #' @param dataset_uid The ID for this dataset.
    #' @param obj_i The index of this data object within the dataset.
    #' @return A list of `routes` and `file_defs` lists.
    get_cell_sets = function(port, dataset_uid, obj_i) {
      retval <- list(
        routes = list(),
        file_defs = list()
      )

      cell_sets_list <- self$create_cell_sets_list()

      retval$routes <- list(
        VitessceConfigServerRoute$new(
          super$get_route(dataset_uid, obj_i, "cell_sets"),
          super$create_response_json(cell_sets_list)
        )
      )
      retval$file_defs <- list(
        list(
          type = DataType$CELL_SETS,
          fileType = FileType$CELL_SETS_JSON,
          url = super$get_url(port, dataset_uid, obj_i, "cell_sets")
        )
      )
      retval
    }
  )
)

#' Seurat DimReduc object wrapper class
#' @title SeuratDimReducWrapper Class
#' @docType class
#' @description
#' Class representing a local DimReduc object in a Vitessce dataset.
#'
#' @rdname SeuratDimReducWrapper
#' @export
SeuratDimReducWrapper <- R6::R6Class("SeuratDimReducWrapper",
   inherit = AbstractWrapper,
   public = list(
     #' @field obj The object to wrap.
     #' @keywords internal
     obj = NULL,
     #' @field mapping A name for the embedding type.
     #' @keywords internal
     mapping = NULL,
     #' @description
     #' Create a wrapper around a Seurat DimReduc object.
     #' @param obj The object to wrap.
     #' @param mapping A name for the embedding type, e.g. "PCA".
     #' @return A new `SeuratDimReducWrapper` object.
     initialize = function(obj, mapping) {
       self$obj <- obj
       self$mapping <- mapping
     },
     #' @description
     #' Create a list representing the cells in the Seurat object.
     #' @return A list that can be converted to JSON.
     #' @keywords internal
     create_cells_list = function() {
       obj <- self$obj
       embedding_name <- self$mapping
       embedding_matrix <- slot(obj, "cell.embeddings")
       cell_ids <- rownames(embedding_matrix)
       cells_list <- obj_list()
       for(cell_id in cell_ids) {
         cells_list[[cell_id]] <- list(
           mappings = obj_list()
         )
       }

       for(cell_id in cell_ids) {
         cells_list[[cell_id]]$mappings[[embedding_name]] <- unname(embedding_matrix[cell_id, 1:2])
       }

       cells_list
     },
     #' @description
     #' Get the routes and file definitions for the cells data type.
     #' @param port The port on which the web server is serving.
     #' @param dataset_uid The ID for this dataset.
     #' @param obj_i The index of this data object within the dataset.
     #' @return A list of `routes` and `file_defs` lists.
     get_cells = function(port, dataset_uid, obj_i) {
       retval <- list(
         routes = list(),
         file_defs = list()
       )

       cells_list <- self$create_cells_list()

       retval$routes <- list(
         VitessceConfigServerRoute$new(
           super$get_route(dataset_uid, obj_i, "cells"),
           super$create_response_json(cells_list)
         )
       )
       retval$file_defs <- list(
         list(
           type = DataType$CELLS,
           fileType = FileType$CELLS_JSON,
           url = super$get_url(port, dataset_uid, obj_i, "cells")
         )
       )
       retval
     }
   )
)
