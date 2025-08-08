library(vitessceR)

test_that("ImageOmeZarrWrapper can be created with required parameters", {
  w <- ImageOmeZarrWrapper$new(img_path = "test.ome.zarr")
  
  expect_equal(w$img_path, "test.ome.zarr")
  expect_false(w$is_remote)
  expect_true(grepl("\\.ome\\.zarr$", w$local_dir_uid))
})

test_that("ImageOmeZarrWrapper requires exactly one of img_path or img_url", {
  expect_error(ImageOmeZarrWrapper$new(), "Expected one of img_path or img_url to be provided")
  expect_error(ImageOmeZarrWrapper$new(img_path = "test.ome.zarr", img_url = "http://example.com/test.ome.zarr"), 
               "Expected one of img_path or img_url to be provided")
})

test_that("ImageOmeZarrWrapper creates remote instance with img_url", {
  w <- ImageOmeZarrWrapper$new(img_url = "http://example.com/test.ome.zarr")
  
  expect_equal(w$img_url, "http://example.com/test.ome.zarr")
  expect_true(w$is_remote)
})

test_that("ImageOmeZarrWrapper get_img_url works for remote", {
  w <- ImageOmeZarrWrapper$new(img_url = "http://example.com/test.ome.zarr")
  
  url <- w$get_img_url("http://localhost:8000", "dataset1", 1)
  expect_equal(url, "http://example.com/test.ome.zarr")
})

test_that("ImageOmeZarrWrapper get_img_url works for local", {
  w <- ImageOmeZarrWrapper$new(img_path = "test.ome.zarr")
  
  url <- w$get_img_url("http://localhost:8000", "dataset1", 1)
  expected_url <- paste0("http://localhost:8000/dataset1/1/", w$local_dir_uid)
  expect_equal(url, expected_url)
})

test_that("ImageOmeZarrWrapper make_file_def_creator creates correct file definition", {
  w <- ImageOmeZarrWrapper$new(img_path = "test.ome.zarr")
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "image.ome-zarr")
  expect_true(grepl("http://localhost:8000/dataset1/1/", file_def$url))
  expect_true(grepl("\\.ome\\.zarr$", file_def$url))
})

test_that("ImageOmeZarrWrapper make_file_def_creator includes coordinate_transformations when provided", {
  transformations <- list(matrix = c(1, 0, 0, 0, 1, 0, 0, 0, 1))
  w <- ImageOmeZarrWrapper$new(img_path = "test.ome.zarr", coordinate_transformations = transformations)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$options$coordinateTransformations, transformations)
})

test_that("ImageOmeZarrWrapper make_file_def_creator includes coordination_values when provided", {
  coord_values <- list(spatialImageLayer = "image")
  w <- ImageOmeZarrWrapper$new(img_path = "test.ome.zarr", coordination_values = coord_values)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$coordinationValues, coord_values)
})

test_that("ObsSegmentationsOmeZarrWrapper can be created with required parameters", {
  w <- ObsSegmentationsOmeZarrWrapper$new(img_path = "segmentations.ome.zarr")
  
  expect_equal(w$img_path, "segmentations.ome.zarr")
  expect_false(w$is_remote)
})

test_that("ObsSegmentationsOmeZarrWrapper requires exactly one of img_path or img_url", {
  expect_error(ObsSegmentationsOmeZarrWrapper$new(), "Expected one of img_path or img_url to be provided")
  expect_error(ObsSegmentationsOmeZarrWrapper$new(img_path = "seg.ome.zarr", img_url = "http://example.com/seg.ome.zarr"), 
               "Expected one of img_path or img_url to be provided")
})

test_that("ObsSegmentationsOmeZarrWrapper make_file_def_creator creates correct file definition", {
  w <- ObsSegmentationsOmeZarrWrapper$new(img_path = "segmentations.ome.zarr")
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "obsSegmentations.ome-zarr")
  expect_true(grepl("http://localhost:8000/dataset1/1/", file_def$url))
})

test_that("ObsSegmentationsOmeZarrWrapper includes obs_types_from_channel_names when provided", {
  w <- ObsSegmentationsOmeZarrWrapper$new(img_path = "segmentations.ome.zarr", obs_types_from_channel_names = TRUE)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_true(file_def$options$obsTypesFromChannelNames)
})

test_that("ObsSegmentationsOmeZarrWrapper includes coordinate_transformations when provided", {
  transformations <- list(matrix = c(1, 0, 0, 0, 1, 0, 0, 0, 1))
  w <- ObsSegmentationsOmeZarrWrapper$new(img_path = "segmentations.ome.zarr", coordinate_transformations = transformations)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$options$coordinateTransformations, transformations)
})