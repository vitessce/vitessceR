#' Coordination Type
#'
#' The \code{CoordinationType} list contains an enumeration of
#' valid string constant values representing coordination types
#' in the coordination space.
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

#' Data Type
#'
#' The \code{DataType} list contains an enumeration of
#' valid string constant values representing data types
#' for dataset files.
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

#' File Type
#'
#' The \code{FileType} list contains an enumeration of
#' valid string constant values representing file types
#' for dataset files.
#'
#' @export
FileType <- list(
  EXPRESSION_MATRIX_ZARR = "expression-matrix.zarr",
  CELLS_JSON = "cells.json",
  MOLECULES_JSON = "molecules.json",
  NEIGHBORHOODS_JSON = "neighborhoods.json",
  RASTER_JSON = "raster.json",
  CELL_SETS_JSON = "cell-sets.json"
)
