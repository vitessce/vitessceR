
#' Subclass of SeuratWrapper to deal with cell sets.
#' @title MyCustomSeuratWrapper Class
#' @docType class
#' @description
#' Subclass of SeuratWrapper.
#'
#' @rdname MyCustomSeuratWrapper
#' @export
MyCustomSeuratWrapper <- R6::R6Class("MyCustomSeuratWrapper",
 inherit = SeuratWrapper,
 public = list(
   #' @field cell_sets The cell sets.
   #' @keywords internal
   cell_sets = NULL,
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
   initialize = function(obj, cell_sets, ...) {
     super$initialize(obj, ...)
     self$cell_sets <- cell_sets
   },
   #' @description
   #' Create a list representing the cluster assignments in the Seurat object.
   #' @return A list that can be converted to JSON.
   #' @keywords internal
   create_cell_sets_list = function() {
     obj <- self$obj

     cells <- Seurat::Idents(obj)

     # https://s3.amazonaws.com/vitessce-data/0.0.31/master_release/linnarsson/linnarsson.cell-sets.json
     cell_sets_list <- list(
       datatype = jsonlite::unbox("cell"),
       version = jsonlite::unbox("0.1.3"),
       tree = list()
     )

     if(!is.na(self$cell_sets)) {
       for(cell_set_name in names(self$cell_sets)) {
         cell_set <- self$cell_sets[[cell_set_name]]

         cell_set_meta_node <- list(
           name = jsonlite::unbox(cell_set_name),
           children = list()
         )
         cell_set_annotations <- cell_set
         cell_set_annotation_scores <- NA

         cluster_names <- sort(unique(cell_set_annotations))

         for(cluster_name in cluster_names) {
           cells_in_cluster <- names(cells[cell_set_annotations == cluster_name])

           # TODO: find out if there is a way to return NULL
           make_null_tuples <- function(x) { list(jsonlite::unbox(x), jsonlite::unbox(NA)) }
           cells_in_cluster_with_score <- purrr::map(cells_in_cluster, make_null_tuples)

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
   }
 )
)
