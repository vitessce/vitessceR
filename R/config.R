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

DataType <- list(
  CELLS = "cells",
  CELL_SETS = "cell-sets",
  EXPRESSION_MATRIX = "expression-matrix",
  MOLECULES = "molecules",
  NEIGHBORHOODS = "neighborhoods",
  RASTER = "raster"
)

FileType <- list(
  EXPRESSION_MATRIX_ZARR = "expression-matrix.zarr",
  CELLS_JSON = "cells.json",
  MOLECULES_JSON = "molecules.json",
  NEIGHBORHOODS_JSON = "neighborhoods.json",
  RASTER_JSON = "raster.json",
  CELL_SETS_JSON = "cell-sets.json"
)


getNextScope <- function(prevScopes) {
  chars <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  getNext <- function(nextCharIndices) {
    r <- c()
    for(charIndex in nextCharIndices) {
      r <- c(chars[charIndex], r)
    }
    increment <- TRUE
    for(i in 1:length(nextCharIndices)) {
      nextCharIndices[i] <- nextCharIndices[i] + 1
      val <- nextCharIndices[i]
      if(val > length(chars)) {
        nextCharIndices[i] <- 1
      } else {
        increment <- FALSE
        break;
      }
    }
    if(increment) {
      nextCharIndices <- c(nextCharIndices, 1)
    }
    newScope <- paste(r, sep = "")
    result <- list(ns = newScope, nci = nextCharIndices)
    result
  }
  nextCharIndices = c(1)

  nextScopeResult <- getNext(nextCharIndices)
  nextScope <- nextScopeResult$ns
  nextCharIndices <- nextScopeResult$nci
  while(is.element(nextScope, prevScopes)) {
    nextScopeResult <- getNext(nextCharIndices)
    nextScope <- nextScopeResult$ns
    nextCharIndices <- nextScopeResult$nci
  }
  nextScope
}

VitessceConfigDatasetFile <- R6Class("VitessceConfigDatasetFile",
  public = list(
    file = NULL,
    initialize = function(url, dataType, fileType) {
      self$file <- list(
        url = url,
        type = dataType,
        fileType = fileType
      )
    },
    toList = function() {
      self$file
    }
  )
)

VitessceConfigDataset <- R6Class("VitessceConfigDataset",
  public = list(
    dataset = NULL,
    objs = NULL,
    initialize = function(uid, name) {
      self$dataset <- list(
        uid = uid,
        name = name,
        files = list()
      )
      self$objs <- list()
    },
    addFile = function(url, dataType, fileType) {
      newFile <- VitessceConfigDatasetFile$new(url, dataType, fileType)
      self$dataset$files <- append(self$dataset$files, newFile)
    },
    addObject = function(obj) {
      self$objs <- append(self$objs, obj)
    },
    toList = function(onObj = NA) {
        objFileDefs <- list()
        if(length(self$objs) > 0) {
          for(i in length(self$objs)) {
            obj <- self$objs[[i]]
            if(!is.na(onObj)) {
              newObjFileDefs <- onObj(obj, self$dataset$uid, i)
              objFileDefs <- append(objFileDefs, newObjFileDefs)
            }
          }
        }

        retval <- self$dataset
        retvalFiles <- list()
        for(f in self$dataset$files) {
          fList <- f$toList()
          retvalFiles <- append(retvalFiles, list(fList))
        }
        for(fList in objFileDefs) {
          retvalFiles <- append(retvalFiles, list(fList))
        }
        retval$files <- retvalFiles
        retval
    }
  )
)

VitessceConfigCoordinationScope <- R6Class("VitessceConfigCoordinationScope",
  public = list(
   cType = NULL,
   cScope = NULL,
   cValue = NULL,
   initialize = function(cType, cScope) {
     self$cType <- cType
     self$cScope <- cScope
     self$cValue <- NA
   },
   setValue = function(cValue) {
     self$cValue <- cValue
     self
   }
  )
)

VitessceConfigView <- R6Class("VitessceConfigView",
  public = list(
    view = NULL,
    initialize = function(component, coordinationScopes, x, y, w, h) {
      self$view <- list(
        component = component,
        coordinationScopes = coordinationScopes,
        x = 0,
        y = 0,
        w = 1,
        h = 1
      )
    },
    useCoordination = function(cScopes) {
      cScopes <- list(1, 2)
      for(cScope in cScopes) {
        self$view$coordinationScopes[[cScope$cType]] = cScope$cScope
      }
      self
    },
    toList = function() {
      self$view
    }
  )
)

VitessceConfig <- R6Class("VitessceConfig",
  public = list(
    config = NULL,
    initialize = function(name = NA, description = NA) {
      self$config <- list(
        version = "1.0.0",
        name = ifelse(is.na(name), "", name),
        description = ifelse(is.na(description), "", description),
        datasets = list(),
        coordinationSpace = list(),
        layout = list(),
        initStrategy = "auto"
      )
    },
    addDataset = function(name) {
      prevDatasetUids <- c()
      for(d in self$config$datasets) {
        prevDatasetUids <- c(prevDatasetUids, d$dataset$uid)
      }
      uid <- getNextScope(prevDatasetUids)
      newDataset <- VitessceConfigDataset$new(uid, name)
      self$config$datasets <- append(self$config$datasets, newDataset)

      newScopes <- self$addCoordination(CoordinationType$DATASET)
      newScopes[[1]]$setValue(uid)
      newDataset
    },
    addView = function(dataset, component, x = NA, y = NA, w = NA, h = NA, mapping = NA) {
      datasetScopeNameMatches <- c()
      for(scopeName in names(self$config$coordinationSpace[[CoordinationType$DATASET]])) {
        datasetScope <- self$config$coordinationSpace[[CoordinationType$DATASET]][[scopeName]]
        if(datasetScope$cValue == dataset$dataset$uid) {
          datasetScopeNameMatches <- c(datasetScopeNameMatches, scopeName)
        }
      }
      datasetScopeName <- NA
      if(length(datasetScopeNameMatches) == 1) {
        datasetScopeName <- datasetScopeNameMatches[1]
      }

      coordinationScopes <- list()
      coordinationScopes[[CoordinationType$DATASET]] <- datasetScopeName
      newView <- VitessceConfigView$new(component, coordinationScopes, x, y, w, h)

      if(!is.na(mapping)) {
        etScopes <- self$addCoordination(CoordinationType.EMBEDDING_TYPE)
        etScope <- etScope[[1]]
        etScope$setValue(mapping)
        newView$useCoordination(etScope)
      }

      self$config$layout <- append(self$config$layout, newView)
      newView
    },
    addCoordination = function(cTypes) {
      result <- list()
      for(cType in cTypes) {
        cObj <- self$config$coordinationSpace[[cType]]
        prevScopes <- ifelse(is.null(cObj), character(), names(cObj))
        cScope <- getNextScope(prevScopes)
        scope <- VitessceConfigCoordinationScope$new(cType, cScope)
        if(!is.element(cType, names(self$config$coordinationSpace))) {
          self$config$coordinationSpace[[cType]] <- list()
        }
        self$config$coordinationSpace[[cType]][[cScope]] <- scope
        result <- append(result, scope)
      }
      result
    },
    layout = function() {
      # TODO
    },
    toList = function(onObj = NA) {
      retval <- self$config

      retvalDatasets <- list()
      for(d in self$config$datasets) {
        dList <- d$toList(onObj)
        retvalDatasets <- append(retvalDatasets, list(dList))
      }
      retval$datasets <- retvalDatasets

      retvalCoordinationSpace <- list()
      for(cType in names(self$config$coordinationSpace)) {
        retvalCoordinationSpace[[cType]] <- list()
        cScopes <- self$config$coordinationSpace[[cType]]
        for(cScopeName in names(cScopes)) {
          cScope <- cScopes[[cScopeName]]
          retvalCoordinationSpace[[cType]][[cScopeName]] <- cScope$cValue
        }
      }
      retval$coordinationSpace <- retvalCoordinationSpace

      retvalLayout <- list()
      for(v in self$config$layout) {
        vList <- v$toList()
        retvalLayout <- append(retvalLayout, list(vList))
      }
      retval$layout <- retvalLayout

      retval
    }
  )
)

#' Vitessce config
#'
#' @param name A name for the view config
#' @param description A description for the view config
#'
#' @import R6
#' @import rjson
#'
#' @export
vitessceConfig <- function(name = "", description = "") {
  return;
}

vitessceConfigFromDict <- function(config) {
  vc <- VitessceConfig$new(config$name, config$description)
  for(d in config$datasets) {
    newDataset <- vc$addDataset(d$uid, d$name)
    for(f in d$files) {
      newDataset$addFile(
        f$url,
        f$type,
        f$fileType
      )
    }
  }
  # TODO: Add each coordination scope from the incoming config.
  # TODO: Add the components (layout) from the incoming config.
  vc
}

vitessceConfigFromObject <- function(obj, name = NA, description = NA) {
  vc <- VitessceConfig$new(name, description)
  ds <- vc$addDataset("From object")
  ds$addObject(obj)
  # TODO: infer views and coordinations
  vc
}
