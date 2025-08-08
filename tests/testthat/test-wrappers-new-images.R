library(vitessceR)

test_that("ImageOmeTiffWrapper can be created with required parameters", {
  w <- ImageOmeTiffWrapper$new(img_path = "test.ome.tif")
  
  expect_equal(w$img_path, "test.ome.tif")
  expect_false(w$is_remote)
  expect_true(grepl("\\.ome\\.tif$", w$local_img_uid))
  expect_true(grepl("\\.offsets\\.json$", w$local_offsets_uid))
})

test_that("ImageOmeTiffWrapper requires exactly one of img_path or img_url", {
  expect_error(ImageOmeTiffWrapper$new(), "Expected one of img_path or img_url to be provided")
  expect_error(ImageOmeTiffWrapper$new(img_path = "test.ome.tif", img_url = "http://example.com/test.ome.tif"), 
               "Expected one of img_path or img_url to be provided")
})

test_that("ImageOmeTiffWrapper allows at most one offsets parameter", {
  expect_error(ImageOmeTiffWrapper$new(img_path = "test.ome.tif", 
                                       offsets_path = "offsets.json", 
                                       offsets_url = "http://example.com/offsets.json"), 
               "Expected zero or one of offsets_path or offsets_url to be provided")
})

test_that("ImageOmeTiffWrapper creates remote instance with img_url", {
  w <- ImageOmeTiffWrapper$new(img_url = "http://example.com/test.ome.tif")
  
  expect_equal(w$img_url, "http://example.com/test.ome.tif")
  expect_true(w$is_remote)
})

test_that("ImageOmeTiffWrapper get_img_url works for remote", {
  w <- ImageOmeTiffWrapper$new(img_url = "http://example.com/test.ome.tif")
  
  url <- w$get_img_url("http://localhost:8000", "dataset1", 1)
  expect_equal(url, "http://example.com/test.ome.tif")
})

test_that("ImageOmeTiffWrapper get_img_url works for local", {
  w <- ImageOmeTiffWrapper$new(img_path = "test.ome.tif")
  
  url <- w$get_img_url("http://localhost:8000", "dataset1", 1)
  expected_url <- paste0("http://localhost:8000/dataset1/1/", w$local_img_uid)
  expect_equal(url, expected_url)
})

test_that("ImageOmeTiffWrapper get_offsets_url returns NA when no offsets provided", {
  w <- ImageOmeTiffWrapper$new(img_path = "test.ome.tif")
  
  url <- w$get_offsets_url("http://localhost:8000", "dataset1", 1)
  expect_true(is.na(url))
})

test_that("ImageOmeTiffWrapper get_offsets_url works with remote offsets", {
  w <- ImageOmeTiffWrapper$new(img_url = "http://example.com/test.ome.tif",
                               offsets_url = "http://example.com/offsets.json")
  
  url <- w$get_offsets_url("http://localhost:8000", "dataset1", 1)
  expect_equal(url, "http://example.com/offsets.json")
})

test_that("ImageOmeTiffWrapper make_file_def_creator creates correct file definition", {
  w <- ImageOmeTiffWrapper$new(img_path = "test.ome.tif")
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "image.ome-tiff")
  expect_true(grepl("http://localhost:8000/dataset1/1/", file_def$url))
  expect_true(grepl("\\.ome\\.tif$", file_def$url))
})

test_that("ImageOmeTiffWrapper make_file_def_creator includes coordinate_transformations when provided", {
  transformations <- list(matrix = c(1, 0, 0, 0, 1, 0, 0, 0, 1))
  w <- ImageOmeTiffWrapper$new(img_path = "test.ome.tif", coordinate_transformations = transformations)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$options$coordinateTransformations, transformations)
})

test_that("ImageOmeTiffWrapper make_file_def_creator includes coordination_values when provided", {
  coord_values <- list(spatialImageLayer = "image")
  w <- ImageOmeTiffWrapper$new(img_path = "test.ome.tif", coordination_values = coord_values)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$coordinationValues, coord_values)
})

test_that("ObsSegmentationsOmeTiffWrapper can be created with required parameters", {
  w <- ObsSegmentationsOmeTiffWrapper$new(img_path = "segmentations.ome.tif")
  
  expect_equal(w$img_path, "segmentations.ome.tif")
  expect_false(w$is_remote)
})

test_that("ObsSegmentationsOmeTiffWrapper make_file_def_creator creates correct file definition", {
  w <- ObsSegmentationsOmeTiffWrapper$new(img_path = "segmentations.ome.tif")
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "obsSegmentations.ome-tiff")
  expect_true(grepl("http://localhost:8000/dataset1/1/", file_def$url))
})

test_that("ObsSegmentationsOmeTiffWrapper includes obs_types_from_channel_names when provided", {
  w <- ObsSegmentationsOmeTiffWrapper$new(img_path = "segmentations.ome.tif", obs_types_from_channel_names = TRUE)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_true(file_def$options$obsTypesFromChannelNames)
})