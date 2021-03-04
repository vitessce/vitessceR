#' Enumeration of Coordination Type values
#'
#' The \code{CoordinationType} list contains an enumeration of
#' valid string constant values representing coordination types
#' in the coordination space.
#' \itemize{
#'   \item `DATASET`: The dataset to be used for a view.
#'   \item `EMBEDDING_TYPE`: The type of embedding to be used in the embedding scatterplot view, for example "UMAP" or "t-SNE".
#'   \item `EMBEDDING_ZOOM`: The zoom level of an embedding scatterplot view.
#'   \item `EMBEDDING_ROTATION`: The rotation of an embedding scatterplot view.
#'   \item `EMBEDDING_TARGET_X`: The x-axis center of an embedding scatterplot view.
#'   \item `EMBEDDING_TARGET_Y`: The y-axis center of an embedding scatterplot view.
#'   \item `EMBEDDING_TARGET_Z`: The z-axis center of an embedding scatterplot view.
#'   \item `SPATIAL_ZOOM`: The zoom level of a spatial view.
#'   \item `SPATIAL_ROTATION`: The rotation of a spatial view.
#'   \item `SPATIAL_TARGET_X`: The x-coordinate of the center of a spatial view.
#'   \item `SPATIAL_TARGET_Y`: The y-coordinate of the center of a spatial view.
#'   \item `SPATIAL_TARGET_Z`: The z-coordinate of the center of a spatial view.
#'   \item `HEATMAP_ZOOM_X`: The x-axis zoom level of a heatmap view.
#'   \item `HEATMAP_ZOOM_Y`: The y-axis zoom level of a heatmap view.
#'   \item `HEATMAP_TARGET_X`: The x-coordinate of the center of a heatmap view.
#'   \item `HEATMAP_TARGET_Y`: The y-coordinate of the center of a heatmap view.
#'   \item `CELL_FILTER`: A subset of cells to include after filtering.
#'   \item `CELL_HIGHLIGHT`: A subset of cells to highlight.
#'   \item `CELL_SET_SELECTION`: A subset of cell sets to select.
#'   \item `CELL_SET_HIGHLIGHT`: A subset of cell sets to highlight.
#'   \item `CELL_SET_COLOR`: A mapping from cell sets to colors.
#'   \item `GENE_FILTER`: A subset of genes to include after filtering.
#'   \item `GENE_HIGHLIGHT`: A subset of genes to highlight.
#'   \item `GENE_SELECTION`: A subset of genes to select.
#'   \item `GENE_EXPRESSION_COLORMAP`: The colormap to use for the gene expression scale.
#'   \item `GENE_EXPRESSION_COLORMAP_RANGE`: The range of gene expression values to map.
#'   \item `CELL_COLOR_ENCODING`: The color encoding to use for cell entities.
#'   \item `SPATIAL_LAYERS`: Layer definitions for the spatial view.
#'   \item `GENOMIC_ZOOM`: The zoom level of a higlass view.
#'   \item `GENOMIC_TARGET_X`: The x-coordinate of the center of a higlass view.
#'   \item `GENOMIC_TARGET_Y`: The y-coordinate of the center of a higlass view.
#'   \item `ADDITIONAL_CELL_SETS`: User-defined cell sets.
#' }
#'
#' @export
CoordinationType <- list(
  DATASET = "dataset",
  EMBEDDING_TYPE = "embeddingType",
  EMBEDDING_ZOOM = "embeddingZoom",
  EMBEDDING_ROTATION = "embeddingRotation",
  EMBEDDING_TARGET_X = "embeddingTargetX",
  EMBEDDING_TARGET_Y = "embeddingTargetY",
  EMBEDDING_TARGET_Z = "embeddingTargetZ",
  SPATIAL_ZOOM = "spatialZoom",
  SPATIAL_ROTATION = "spatialRotation",
  SPATIAL_TARGET_X = "spatialTargetX",
  SPATIAL_TARGET_Y = "spatialTargetY",
  SPATIAL_TARGET_Z = "spatialTargetZ",
  HEATMAP_ZOOM_X = "heatmapZoomX",
  HEATMAP_ZOOM_Y = "heatmapZoomY",
  HEATMAP_TARGET_X = "heatmapTargetX",
  HEATMAP_TARGET_Y = "heatmapTargetY",
  CELL_FILTER = "cellFilter",
  CELL_HIGHLIGHT = "cellHighlight",
  CELL_SET_SELECTION = "cellSetSelection",
  CELL_SET_HIGHLIGHT = "cellSetHighlight",
  CELL_SET_COLOR = "cellSetColor",
  GENE_FILTER = "geneFilter",
  GENE_HIGHLIGHT = "geneHighlight",
  GENE_SELECTION = "geneSelection",
  GENE_EXPRESSION_COLORMAP = "geneExpressionColormap",
  GENE_EXPRESSION_COLORMAP_RANGE = "geneExpressionColormapRange",
  CELL_COLOR_ENCODING = "cellColorEncoding",
  SPATIAL_LAYERS = "spatialLayers",
  GENOMIC_ZOOM = "genomicZoom",
  GENOMIC_TARGET_X = "genomicTargetX",
  GENOMIC_TARGET_Y = "genomicTargetY",
  ADDITIONAL_CELL_SETS = "additionalCellSets"
)

#' Enumeration of Data Type values
#'
#' The \code{DataType} list contains an enumeration of
#' valid string constant values representing data types
#' for dataset files.
#' \itemize{
#'   \item `CELLS`: The `cells` data type.
#'   \item `CELL_SETS`: The `cell-sets` data type.
#'   \item `EXPRESSION_MATRIX`: The `expression-matrix` data type.
#'   \item `MOLECULES`: The `molecules` data type.
#'   \item `NEIGHBORHOODS`: The `neighborhoods` data type.
#'   \item `RASTER`: The `raster` data type.
#' }
#'
#' @export
DataType <- list(
  CELLS = "cells",
  CELL_SETS = "cell-sets",
  EXPRESSION_MATRIX = "expression-matrix",
  MOLECULES = "molecules",
  NEIGHBORHOODS = "neighborhoods",
  RASTER = "raster"
)

#' Enumeration of File Type values
#'
#' The \code{FileType} list contains an enumeration of
#' valid string constant values representing file types
#' for dataset files.
#' \itemize{
#'   \item `CELLS_JSON`: The `cells.json` file type.
#'   \item `MOLECULES_JSON`: The `molecules.json` file type.
#'   \item `NEIGHBORHOODS_JSON`: The `neighborhoods.json` file type.
#'   \item `RASTER_JSON`: The `raster.json` file type.
#'   \item `CELL_SETS_JSON`: The `cell-sets.json` file type.
#'   \item `CLUSTERS_JSON`: The `clusters.json` file type.
#'   \item `EXPRESSION_MATRIX_ZARR`: The `expression-matrix.zarr` file type.
#' }
#'
#' @export
FileType <- list(
  CELLS_JSON = "cells.json",
  MOLECULES_JSON = "molecules.json",
  NEIGHBORHOODS_JSON = "neighborhoods.json",
  RASTER_JSON = "raster.json",
  CELL_SETS_JSON = "cell-sets.json",
  CLUSTERS_JSON = "clusters.json",
  EXPRESSION_MATRIX_ZARR = "expression-matrix.zarr"
)

#' Enumeration of Component values
#'
#' The \code{Component} list contains an enumeration of
#' valid string constant values representing components.
#' \itemize{
#'   \item `SCATTERPLOT`: The `scatterplot` component.
#'   \item `SPATIAL`: The `spatial` component.
#'   \item `DESCRIPTION`: The `description` component.
#'   \item `STATUS`: The `status` component.
#'   \item `CELL_SETS`: The `cellSets` component.
#'   \item `HEATMAP`: The `heatmap` component.
#'   \item `LAYER_CONTROLLER`: The `layerController` component.
#'   \item `CELL_SET_SIZES`: The `cellSetSizes` component.
#' }
#'
#' @export
Component <- list(
  SCATTERPLOT = "scatterplot",
  SPATIAL = "spatial",
  DESCRIPTION = "description",
  STATUS = "status",
  CELL_SETS = "cellSets",
  HEATMAP = "heatmap",
  LAYER_CONTROLLER = "layerController",
  HIGLASS = "higlass",
  CELL_SET_SIZES = "cellSetSizes"
)
