library(vitessce)

test_that("VitessceConfig new", {
  vc <- VitessceConfig$new("My config")

  vc_list <- vc$to_list()
  expect_equal(vc_list, list(
    version = "1.0.0",
    name = "My config",
    description = "",
    datasets = list(),
    coordinationSpace = list(),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfig add_dataset", {
  vc <- VitessceConfig$new("My config")
  vc$add_dataset("My dataset")

  vc_list <- vc$to_list()
  expect_equal(vc_list, list(
    version = "1.0.0",
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

  vc_list <- vc$to_list()
  expect_equal(vc_list, list(
    version = "1.0.0",
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
        A = "A"
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

  vc_list <- vc$to_list()
  expect_equal(vc_list, list(
    version = "1.0.0",
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
        A = "A"
      )
    ),
    layout = list(),
    initStrategy = "auto"
  ))
})

test_that("VitessceConfigDataset add_object", {
  vc <- VitessceConfig$new("My config")
  ds <- vc$add_dataset("My dataset")
  ds$add_object("Test")

  on_obj <- function(obj, dataset_uid, obj_i) {
    retval <- list(
      list(
        url = "http://localhost:8000/cells",
        type = "cells",
        fileType = "cells.json"
      )
    )
  }

  vc_list <- vc$to_list(on_obj)
  expect_equal(vc_list, list(
    version = "1.0.0",
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
        A = "A"
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

  vc_list <- vc$to_list()
  expect_equal(vc_list, list(
    version = "1.0.0",
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
      embeddingType = list(
        A = "UMAP"
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

  vc_list <- vc$to_list()
  expect_equal(vc_list, list(
    version = "1.0.0",
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
  ))
})
