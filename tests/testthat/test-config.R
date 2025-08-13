library(vitessceR)

test_that("VitessceConfig new", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(),
    coordinationSpace = obj_list(),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfig add_dataset", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  vc$add_dataset("My dataset")

  vc_list <- vc$to_list(base_url = "http://localhost:8000")

  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list()
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      )
    ),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfigDataset add_file", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")
  ds$add_file(url = "http://example.com/cells", file_type = "cells.json")

  vc_list <- vc$to_list(base_url = "http://localhost:8000")

  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list(
          list(
            fileType = "cells.json",
            url = "http://example.com/cells"
          )
        )
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      )
    ),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfigDataset add_file twice", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")
  ds$add_file(
    url = "http://example.com/cells", file_type = "cells.json"
  )$add_file(
    url = "http://example.com/molecules", file_type = "molecules.json"
  )

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list(
          list(
            fileType = "cells.json",
            url = "http://example.com/cells"
          ),
          list(
            fileType = "molecules.json",
            url = "http://example.com/molecules"
          )
        )
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      )
    ),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfigDataset add_file with options", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")

  file_options = obj_list(
    schemaVersion = "0.0.2",
    images = list(
        obj_list(
          name = "Image",
          type = "ome-tiff",
          url = "https://vitessce-demo-data.storage.googleapis.com/exemplar-001/exemplar-001.pyramid.ome.tif"
        )
    ),
    renderLayers = list(
      "Image"
    )
  )
  ds$add_file(file_type = "cells.json", options = file_options)

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list(
          list(
            fileType = "cells.json",
            options = file_options
          )
        )
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      )
    ),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfigDataset add_object", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")

  MockWrapper <- R6::R6Class("SeuratWrapper",
     inherit = AbstractWrapper,
     public = list(
       convert_and_save = function(dataset_uid, obj_i, base_dir = NA) {
         get_cells <- function(base_url) {
           return(list(
             url = "http://localhost:8000/cells",
             fileType = "cells.json"
           ))
         }
         self$file_def_creators <- append(self$file_def_creators, get_cells)
       }
     )
  )

  obj <- MockWrapper$new()
  ds$add_object(obj)

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list(
          list(
            url = "http://localhost:8000/cells",
            fileType = "cells.json"
          )
        )
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      )
    ),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfig add_view", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")
  v1 <- vc$add_view(ds, "spatial")
  v2 <- vc$add_view(ds, "scatterplot", mapping = "UMAP")

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list()
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      ),
      embeddingType = list(
        A = jsonlite::unbox("UMAP")
      )
    ),
    layout = list(
      list(
        component = "spatial",
        coordinationScopes = list(
          dataset = "A"
        ),
        x = 0, y = 0, w = 1, h = 1
      ),
      list(
        component = "scatterplot",
        coordinationScopes = list(
          dataset = "A",
          embeddingType = "A"
        ),
        x = 0, y = 0, w = 1, h = 1
      )
    ),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfig add_coordination", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")
  v1 <- vc$add_view(ds, "spatial")
  v2 <- vc$add_view(ds, "spatial")

  c_scopes <- vc$add_coordination(c("spatialZoom", "spatialTargetX"))
  c_scopes[[1]]$set_value(10)
  c_scopes[[2]]$set_value(20)
  v1$use_coordination(c_scopes)
  v2$use_coordination(c_scopes)

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list()
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      ),
      spatialZoom = list(
        A = jsonlite::unbox(10)
      ),
      spatialTargetX = list(
        A = jsonlite::unbox(20)
      )
    ),
    layout = list(
      list(
        component = "spatial",
        coordinationScopes = list(
          dataset = "A",
          spatialZoom = "A",
          spatialTargetX = "A"
        ),
        x = 0, y = 0, w = 1, h = 1
      ),
      list(
        component = "spatial",
        coordinationScopes = list(
          dataset = "A",
          spatialZoom = "A",
          spatialTargetX = "A"
        ),
        x = 0, y = 0, w = 1, h = 1
      )
    ),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfig layout", {
  vc <- VitessceConfig$new(schema_version = "1.0.9", name = "My config")
  ds <- vc$add_dataset("My dataset")
  v1 <- vc$add_view(ds, "spatial")
  v2 <- vc$add_view(ds, "description")
  v3 <- vc$add_view(ds, "layerController")

  vc$layout(hconcat(v1, vconcat(v2, v3)))

  vc_list <- vc$to_list(base_url = "http://localhost:8000")
  expect_equal(vc_list, list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list()
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = jsonlite::unbox("A")
      )
    ),
    layout = list(
      list(
        component = "spatial",
        coordinationScopes = list(
          dataset = "A"
        ),
        x = 0, y = 0, w = 6, h = 12
      ),
      list(
        component = "description",
        coordinationScopes = list(
          dataset = "A"
        ),
        x = 6, y = 0, w = 6, h = 6
      ),
      list(
        component = "layerController",
        coordinationScopes = list(
          dataset = "A"
        ),
        x = 6, y = 6, w = 6, h = 6
      )
    ),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfig from list", {
  vc_list_orig <- list(
    version = "1.0.9",
    name = "My config",
    description = "",
    datasets = list(
      list(
        uid = "A",
        name = "My dataset",
        files = list()
      )
    ),
    coordinationSpace = list(
      dataset = list(
        A = "A"
      ),
      spatialZoom = list(
        A = 10
      ),
      spatialTargetX = list(
        A = 20
      )
    ),
    layout = list(
      list(
        component = "spatial",
        coordinationScopes = list(
          dataset = "A",
          spatialZoom = "A",
          spatialTargetX = "A"
        ),
        x = 0, y = 0, w = 1, h = 1
      ),
      list(
        component = "spatial",
        coordinationScopes = list(
          dataset = "A",
          spatialZoom = "A",
          spatialTargetX = "A"
        ),
        x = 0, y = 0, w = 1, h = 1
      )
    ),
    initStrategy = "auto"
  )

  vc <- VitessceConfig$from_list(vc_list_orig)

  vc_list_loaded <- vc$to_list(base_url = "http://localhost:8000")
  vc_list_orig[['coordinationSpace']][['dataset']][['A']] <- jsonlite::unbox("A")
  vc_list_orig[['coordinationSpace']][['spatialZoom']][['A']] <- jsonlite::unbox(10)
  vc_list_orig[['coordinationSpace']][['spatialTargetX']][['A']] <- jsonlite::unbox(20)
  expect_equal(vc_list_loaded, vc_list_orig)
})

test_that("VitessceConfig link_views_by_dict basic functionality", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "Test config")
  ds <- vc$add_dataset("Test dataset")
  v1 <- vc$add_view(ds, "spatial")
  v2 <- vc$add_view(ds, "scatterplot")

  # Test simple coordination with meta = FALSE
  simple_input <- list()
  simple_input[[CoordinationType$SPATIAL_ZOOM]] <- 2
  simple_input[[CoordinationType$SPATIAL_TARGET_X]] <- 0
  simple_input[[CoordinationType$SPATIAL_TARGET_Y]] <- 0

  vc$link_views_by_dict(list(v1, v2), simple_input, meta = FALSE)

  vc_list <- vc$to_list()
  
  # Check that coordination scopes were created
  expect_true("spatialZoom" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetX" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetY" %in% names(vc_list$coordinationSpace))
  
  # Check that values were set correctly
  zoom_scope_name <- names(vc_list$coordinationSpace$spatialZoom)[1]
  expect_equal(vc_list$coordinationSpace$spatialZoom[[zoom_scope_name]], jsonlite::unbox(2))
  
  x_scope_name <- names(vc_list$coordinationSpace$spatialTargetX)[1]
  expect_equal(vc_list$coordinationSpace$spatialTargetX[[x_scope_name]], jsonlite::unbox(0))
  
  y_scope_name <- names(vc_list$coordinationSpace$spatialTargetY)[1]
  expect_equal(vc_list$coordinationSpace$spatialTargetY[[y_scope_name]], jsonlite::unbox(0))
  
  # Check that views use the coordination scopes
  expect_equal(vc_list$layout[[1]]$coordinationScopes$spatialZoom, zoom_scope_name)
  expect_equal(vc_list$layout[[1]]$coordinationScopes$spatialTargetX, x_scope_name)
  expect_equal(vc_list$layout[[1]]$coordinationScopes$spatialTargetY, y_scope_name)
  
  expect_equal(vc_list$layout[[2]]$coordinationScopes$spatialZoom, zoom_scope_name)
  expect_equal(vc_list$layout[[2]]$coordinationScopes$spatialTargetX, x_scope_name)
  expect_equal(vc_list$layout[[2]]$coordinationScopes$spatialTargetY, y_scope_name)
})

test_that("VitessceConfig link_views_by_dict with meta coordination", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "Test config")
  ds <- vc$add_dataset("Test dataset")
  v1 <- vc$add_view(ds, "spatial")
  v2 <- vc$add_view(ds, "scatterplot")

  # Test with meta coordination (default behavior)
  simple_input <- list()
  simple_input[[CoordinationType$SPATIAL_ZOOM]] <- 3

  vc$link_views_by_dict(list(v1, v2), simple_input)  # meta = TRUE by default

  vc_list <- vc$to_list()
  
  # Check that meta coordination scopes were created
  expect_true("metaCoordinationScopes" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$coordinationSpace))
  
  # Check that views use meta coordination
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[1]]$coordinationScopes))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$layout[[1]]$coordinationScopes))
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[2]]$coordinationScopes))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$layout[[2]]$coordinationScopes))
})

test_that("VitessceConfig add_coordination_by_dict", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "Test config")
  
  # Test add_coordination_by_dict alone
  input_val <- list()
  input_val[[CoordinationType$SPATIAL_ZOOM]] <- 5
  input_val[[CoordinationType$SPATIAL_TARGET_X]] <- 10
  
  result <- vc$add_coordination_by_dict(input_val)
  
  # Check structure of result
  expect_true("spatialZoom" %in% names(result))
  expect_true("spatialTargetX" %in% names(result))
  expect_true("scope" %in% names(result$spatialZoom))
  expect_true("scope" %in% names(result$spatialTargetX))
  
  # Check that scopes were created with correct values
  expect_equal(as.numeric(result$spatialZoom$scope$c_value), 5)
  expect_equal(as.numeric(result$spatialTargetX$scope$c_value), 10)
  expect_equal(result$spatialZoom$scope$c_type, "spatialZoom")
  expect_equal(result$spatialTargetX$scope$c_type, "spatialTargetX")
})

test_that("VitessceConfig add_meta_coordination", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "Test config")
  
  meta_scope <- vc$add_meta_coordination()
  
  # Check that meta scope object was created
  expect_true(inherits(meta_scope, "VitessceConfigMetaCoordinationScope"))
  expect_true(inherits(meta_scope$meta_scope, "VitessceConfigCoordinationScope"))
  expect_true(inherits(meta_scope$meta_by_scope, "VitessceConfigCoordinationScope"))
  
  # Check that coordination space was updated
  vc_list <- vc$to_list()
  expect_true("metaCoordinationScopes" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$coordinationSpace))
})

test_that("VitessceConfig add_coordination_by_dict with hierarchical structure", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "Test config")
  
  # Test complex hierarchical coordination using available coordination types
  # Simulate the Python test structure with R-compatible types
  scopes <- vc$add_coordination_by_dict(list(
    spatialLayers = CL(list(
      list(
        geneFilter = list("BRCA1", "TP53"),
        spatialZoom = 2.5,
        cellFilter = CL(list(
          list(
            spatialTargetX = 100,
            cellSetColor = list(255, 0, 0)
          ),
          list(
            spatialTargetX = 200,
            cellSetColor = list(0, 255, 0)
          )
        ))
      )
    ))
  ))
  
  vc_list <- vc$to_list()
  
  # Check that the hierarchical structure was created
  expect_true("spatialLayers" %in% names(vc_list$coordinationSpace))
  expect_true("geneFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialZoom" %in% names(vc_list$coordinationSpace))
  expect_true("cellFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetX" %in% names(vc_list$coordinationSpace))
  expect_true("cellSetColor" %in% names(vc_list$coordinationSpace))
  
  # Check that dummy values were set for list types
  spatial_layers_scope <- names(vc_list$coordinationSpace$spatialLayers)[1]
  expect_equal(vc_list$coordinationSpace$spatialLayers[[spatial_layers_scope]], jsonlite::unbox("__dummy__"))
  
  # Check that regular values were set correctly
  zoom_scope <- names(vc_list$coordinationSpace$spatialZoom)[1]
  expect_equal(vc_list$coordinationSpace$spatialZoom[[zoom_scope]], jsonlite::unbox(2.5))
  
  # Check gene filter list
  gene_scope <- names(vc_list$coordinationSpace$geneFilter)[1]
  expect_equal(vc_list$coordinationSpace$geneFilter[[gene_scope]], list("BRCA1", "TP53"))
  
  # Check spatial target values
  x_scopes <- names(vc_list$coordinationSpace$spatialTargetX)
  expect_length(x_scopes, 2)
  expect_equal(vc_list$coordinationSpace$spatialTargetX[[x_scopes[1]]], jsonlite::unbox(100))
  expect_equal(vc_list$coordinationSpace$spatialTargetX[[x_scopes[2]]], jsonlite::unbox(200))
  
  # Check color values
  color_scopes <- names(vc_list$coordinationSpace$cellSetColor)
  expect_length(color_scopes, 2)
  expect_equal(vc_list$coordinationSpace$cellSetColor[[color_scopes[1]]], list(255, 0, 0))
  expect_equal(vc_list$coordinationSpace$cellSetColor[[color_scopes[2]]], list(0, 255, 0))
})

test_that("VitessceConfig add_and_use_coordination_by_dict", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
  dataset <- vc$add_dataset(name = "My dataset")

  # Add coordination first
  color_scope_list <- vc$add_coordination("cellSetColor")
  color_scope <- color_scope_list[[1]]
  color_scope$set_value(list(255, 0, 0))

  # Add hierarchical coordination using available R types
  scopes <- vc$add_coordination_by_dict(list(
    spatialLayers = CL(list(
      list(
        geneFilter = list("GENE1", "GENE2"),
        spatialZoom = 1.5,
        cellFilter = CL(list(
          list(
            spatialTargetX = 0,
            cellSetColor = list(0, 255, 0)
          ),
          list(
            spatialTargetX = 1,
            cellSetColor = list(0, 0, 255)
          )
        ))
      )
    )),
    cellHighlight = CL(list(
      list(
        geneSelection = list("GENE3"),
        spatialTargetY = 10,
        cellSetColor = color_scope
      )
    ))
  ))

  spatial_view <- vc$add_view("spatial", dataset = dataset)
  spatial_view$use_coordination_by_dict(scopes)

  vc_list <- vc$to_list()

  # Check dataset
  expect_equal(vc_list$datasets[[1]]$uid, "A")
  expect_equal(vc_list$datasets[[1]]$name, "My dataset")

  # Check coordination space structure
  expect_true("dataset" %in% names(vc_list$coordinationSpace))
  expect_true("spatialLayers" %in% names(vc_list$coordinationSpace))
  expect_true("geneFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialZoom" %in% names(vc_list$coordinationSpace))
  expect_true("cellFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetX" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetY" %in% names(vc_list$coordinationSpace))
  expect_true("cellSetColor" %in% names(vc_list$coordinationSpace))
  expect_true("cellHighlight" %in% names(vc_list$coordinationSpace))
  expect_true("geneSelection" %in% names(vc_list$coordinationSpace))

  # Check specific values
  expect_equal(vc_list$coordinationSpace$dataset[["A"]], jsonlite::unbox("A"))
  
  # Check that spatial layers has dummy value
  spatial_layers_scope <- names(vc_list$coordinationSpace$spatialLayers)[1]
  expect_equal(vc_list$coordinationSpace$spatialLayers[[spatial_layers_scope]], jsonlite::unbox("__dummy__"))
  
  # Check zoom value
  zoom_scope <- names(vc_list$coordinationSpace$spatialZoom)[1]
  expect_equal(vc_list$coordinationSpace$spatialZoom[[zoom_scope]], jsonlite::unbox(1.5))
  
  # Check that color scope value is shared
  color_scopes <- names(vc_list$coordinationSpace$cellSetColor)
  expect_true(any(sapply(color_scopes, function(scope) {
    identical(vc_list$coordinationSpace$cellSetColor[[scope]], list(255, 0, 0))
  })))

  # Check layout - view should use the scopes  
  expect_equal(vc_list$layout[[1]]$component, "spatial")
  expect_true("dataset" %in% names(vc_list$layout[[1]]$coordinationScopes))
  expect_equal(vc_list$layout[[1]]$coordinationScopes$dataset, "A")
})

test_that("VitessceConfig use_meta_complex_coordination", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
  dataset <- vc$add_dataset(name = "My dataset")

  # Create complex hierarchical coordination structure
  scopes <- vc$add_coordination_by_dict(list(
    spatialLayers = CL(list(
      list(
        geneFilter = list("GENE_A", "GENE_B"),
        spatialZoom = 2.0,
        cellFilter = CL(list(
          list(
            spatialTargetX = 0,
            cellSetColor = list(255, 0, 0)
          ),
          list(
            spatialTargetX = 1,
            cellSetColor = list(0, 255, 0)
          )
        ))
      )
    )),
    cellHighlight = CL(list(
      list(
        geneSelection = list("GENE_C"),
        spatialTargetY = 5,
        cellSetColor = list(255, 0, 0)
      )
    ))
  ))

  meta_coordination_scope <- vc$add_meta_coordination()
  meta_coordination_scope$use_coordination_by_dict(scopes)

  spatial_view <- vc$add_view("spatial", dataset = dataset)
  lc_view <- vc$add_view("layerController", dataset = dataset)

  spatial_view$use_meta_coordination(meta_coordination_scope)
  lc_view$use_meta_coordination(meta_coordination_scope)

  vc_list <- vc$to_list()

  # Check basic structure
  expect_equal(vc_list$version, "1.0.16")
  expect_equal(vc_list$name, "My config")
  expect_equal(vc_list$datasets[[1]]$uid, "A")
  expect_equal(vc_list$datasets[[1]]$name, "My dataset")

  # Check coordination space
  expect_true("dataset" %in% names(vc_list$coordinationSpace))
  expect_true("spatialLayers" %in% names(vc_list$coordinationSpace))
  expect_true("geneFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialZoom" %in% names(vc_list$coordinationSpace))
  expect_true("cellFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetX" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetY" %in% names(vc_list$coordinationSpace))
  expect_true("cellSetColor" %in% names(vc_list$coordinationSpace))
  expect_true("cellHighlight" %in% names(vc_list$coordinationSpace))
  expect_true("geneSelection" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopes" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$coordinationSpace))

  # Check meta coordination structure
  meta_scope_names <- names(vc_list$coordinationSpace$metaCoordinationScopes)
  expect_length(meta_scope_names, 1)
  meta_scope_content <- vc_list$coordinationSpace$metaCoordinationScopes[[meta_scope_names[1]]]
  expect_true("spatialLayers" %in% names(meta_scope_content))
  expect_true("cellHighlight" %in% names(meta_scope_content))

  # Check that views use meta coordination
  expect_equal(vc_list$layout[[1]]$component, "spatial")
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[1]]$coordinationScopes))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$layout[[1]]$coordinationScopes))
  
  expect_equal(vc_list$layout[[2]]$component, "layerController")
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[2]]$coordinationScopes))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$layout[[2]]$coordinationScopes))
})

test_that("VitessceConfig link_views_by_dict complex hierarchical", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
  dataset <- vc$add_dataset(name = "My dataset")

  spatial_view <- vc$add_view("spatial", dataset = dataset)
  lc_view <- vc$add_view("layerController", dataset = dataset)

  # Use complex hierarchical coordination linking
  vc$link_views_by_dict(list(spatial_view, lc_view), list(
    spatialLayers = CL(list(
      list(
        geneFilter = list("GENE_1", "GENE_2"),
        spatialZoom = 3.0,
        cellFilter = CL(list(
          list(
            spatialTargetX = 0,
            cellSetColor = list(255, 0, 0)
          ),
          list(
            spatialTargetX = 1,
            cellSetColor = list(0, 255, 0)
          )
        ))
      )
    )),
    cellHighlight = CL(list(
      list(
        geneSelection = list("GENE_3"),
        spatialTargetY = 0,
        cellSetColor = list(255, 0, 0)
      )
    ))
  ))

  vc_list <- vc$to_list()

  # Check structure matches expected format
  expect_equal(vc_list$version, "1.0.16")
  expect_equal(vc_list$name, "My config")
  expect_equal(vc_list$datasets[[1]]$uid, "A")
  expect_equal(vc_list$datasets[[1]]$name, "My dataset")

  # Check coordination space contains all expected types
  expect_true("dataset" %in% names(vc_list$coordinationSpace))
  expect_true("spatialLayers" %in% names(vc_list$coordinationSpace))
  expect_true("geneFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialZoom" %in% names(vc_list$coordinationSpace))
  expect_true("cellFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetX" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetY" %in% names(vc_list$coordinationSpace))
  expect_true("cellSetColor" %in% names(vc_list$coordinationSpace))
  expect_true("cellHighlight" %in% names(vc_list$coordinationSpace))
  expect_true("geneSelection" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopes" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$coordinationSpace))

  # Check specific values
  zoom_scope <- names(vc_list$coordinationSpace$spatialZoom)[1]
  expect_equal(vc_list$coordinationSpace$spatialZoom[[zoom_scope]], jsonlite::unbox(3.0))
  
  gene_filter_scope <- names(vc_list$coordinationSpace$geneFilter)[1]
  expect_equal(vc_list$coordinationSpace$geneFilter[[gene_filter_scope]], list("GENE_1", "GENE_2"))
  
  # Check that both views use meta coordination
  expect_equal(vc_list$layout[[1]]$component, "spatial")
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[1]]$coordinationScopes))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$layout[[1]]$coordinationScopes))
  
  expect_equal(vc_list$layout[[2]]$component, "layerController")
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[2]]$coordinationScopes))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$layout[[2]]$coordinationScopes))
})

test_that("VitessceConfig link_views_by_dict with scope_prefix", {
  vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
  dataset <- vc$add_dataset(name = "My dataset")

  spatial_view <- vc$add_view("spatial", dataset = dataset)
  lc_view <- vc$add_view("layerController", dataset = dataset)

  # Test with scope prefix like Python test
  vc$link_views_by_dict(list(spatial_view, lc_view), list(
    spatialLayers = CL(list(
      list(
        spatialZoom = 1.0,
        cellFilter = CL(list(
          list(
            spatialTargetX = 0,
            cellSetColor = list(255, 0, 0)
          ),
          list(
            spatialTargetX = 1,
            cellSetColor = list(0, 255, 0)
          )
        ))
      )
    ))
  ), scope_prefix = "SOME_PREFIX_")

  vc_list <- vc$to_list()

  # Check structure
  expect_equal(vc_list$version, "1.0.16")
  expect_equal(vc_list$name, "My config")
  expect_equal(vc_list$datasets[[1]]$uid, "A")
  expect_equal(vc_list$datasets[[1]]$name, "My dataset")

  # Check coordination space with prefixes
  expect_true("dataset" %in% names(vc_list$coordinationSpace))
  expect_true("spatialLayers" %in% names(vc_list$coordinationSpace))
  expect_true("spatialZoom" %in% names(vc_list$coordinationSpace))
  expect_true("cellFilter" %in% names(vc_list$coordinationSpace))
  expect_true("spatialTargetX" %in% names(vc_list$coordinationSpace))
  expect_true("cellSetColor" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopes" %in% names(vc_list$coordinationSpace))
  expect_true("metaCoordinationScopesBy" %in% names(vc_list$coordinationSpace))

  # Check that scope names use the prefix
  spatial_layers_scopes <- names(vc_list$coordinationSpace$spatialLayers)
  expect_true(any(grepl("^SOME_PREFIX_", spatial_layers_scopes)))
  
  zoom_scopes <- names(vc_list$coordinationSpace$spatialZoom)
  expect_true(any(grepl("^SOME_PREFIX_", zoom_scopes)))
  
  x_target_scopes <- names(vc_list$coordinationSpace$spatialTargetX)
  expect_true(any(grepl("^SOME_PREFIX_", x_target_scopes)))
  
  color_scopes <- names(vc_list$coordinationSpace$cellSetColor)
  expect_true(any(grepl("^SOME_PREFIX_", color_scopes)))

  # Check meta coordination scope names use prefix
  meta_scopes <- names(vc_list$coordinationSpace$metaCoordinationScopes)
  expect_true(any(grepl("^SOME_PREFIX_", meta_scopes)))

  # Check that views use the prefixed meta coordination
  expect_equal(vc_list$layout[[1]]$component, "spatial")
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[1]]$coordinationScopes))
  meta_scope_used <- vc_list$layout[[1]]$coordinationScopes$metaCoordinationScopes[1]
  expect_true(grepl("^SOME_PREFIX_", meta_scope_used))
  
  expect_equal(vc_list$layout[[2]]$component, "layerController")
  expect_true("metaCoordinationScopes" %in% names(vc_list$layout[[2]]$coordinationScopes))
  meta_scope_used_2 <- vc_list$layout[[2]]$coordinationScopes$metaCoordinationScopes[1]
  expect_true(grepl("^SOME_PREFIX_", meta_scope_used_2))
})
