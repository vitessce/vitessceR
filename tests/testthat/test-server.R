library(vitessce)

test_that("VitessceConfigServer on_obj", {
  server <- VitessceConfigServer$new(8000)

  mock_obj <- list(
    get_data = function(data_type, port, dataset_uid, obj_i) {
      obj_results <- NA
      if(data_type == "cells") {
        obj_results <- list(
          file_defs = list(
            list(
              url = "http://localhost:8000/test",
              type = "cells",
              fileType = "cells.json"
            )
          ),
          routes = list(
            VitessceConfigServerRoute$new("/test", function(req, res) {
              list(hello = "world")
            })
          )
        )
      }
      obj_results
    }
  )

  obj_file_defs <- server$on_obj(mock_obj, "A", 1)

  expect_equal(obj_file_defs, list(
    list(
      url = "http://localhost:8000/test",
      type = "cells",
      fileType = "cells.json"
    )
  ))
})
