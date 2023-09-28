library(vitessceR)

test_that("OmeTiffWrapper", {
  w <- OmeTiffWrapper$new(img_path = "tests/data/test.ome.tiff", name = "Test")
  w$local_img_uid <- "test.ome.tiff"

  file_def_creator <- w$make_raster_file_def_creator("A", "0")
  file_def <- file_def_creator("http://localhost:8000")

  expect_equal(file_def, list(
    fileType = "raster.json",
    options = list(
      schemaVersion = "0.0.2",
      images = list(
        obj_list(
          name = "Test",
          type = "ome-tiff",
          url = "http://localhost:8000/A/0/test.ome.tiff",
          metadata = list(
            isBitmask = FALSE
          )
        )
      )
    )
  ))
})
