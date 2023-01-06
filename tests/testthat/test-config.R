library(vitessceR)

test_that("VitessceConfig new", {
  vc <- VitessceConfig$new("My config")

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
  vc <- VitessceConfig$new("My config")
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
  vc <- VitessceConfig$new("My config")
  ds <- vc$add_dataset("My dataset")
  ds$add_file("http://example.com/cells", "cells", "cells.json")

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
            url = "http://example.com/cells",
            type = "cells",
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

test_that("VitessceConfigDataset add_file twice", {
  vc <- VitessceConfig$new("My config")
  ds <- vc$add_dataset("My dataset")
  ds$add_file("http://example.com/cells", "cells", "cells.json")$add_file("http://example.com/molecules", "molecules", "molecules.json")

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
            url = "http://example.com/cells",
            type = "cells",
            fileType = "cells.json"
          ),
          list(
            url = "http://example.com/molecules",
            type = "molecules",
            fileType = "molecules.json"
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
  vc <- VitessceConfig$new("My config")
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
  ds$add_file(data_type = "cells", file_type = "cells.json", options = file_options)

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
            type = "cells",
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
  vc <- VitessceConfig$new("My config")
  ds <- vc$add_dataset("My dataset")

  MockWrapper <- R6::R6Class("SeuratWrapper",
     inherit = AbstractWrapper,
     public = list(
       convert_and_save = function(dataset_uid, obj_i) {
         get_cells <- function(base_url) {
           return(list(
             url = "http://localhost:8000/cells",
             type = "cells",
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
            type = "cells",
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
  vc <- VitessceConfig$new("My config")
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
  vc <- VitessceConfig$new("My config")
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
  vc <- VitessceConfig$new("My config")
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
