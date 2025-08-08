library(vitessceR)

test_that("SpatialDataWrapper can be created with required parameters", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr")
  
  expect_equal(w$sdata_path, "test.sdata.zarr")
  expect_false(w$is_remote)
  expect_true(grepl("\\.sdata\\.zarr$", w$local_dir_uid))
  expect_equal(w$table_path, "tables/table")
})

test_that("SpatialDataWrapper requires exactly one of sdata_path or sdata_url", {
  expect_error(SpatialDataWrapper$new(), "Expected one of sdata_path or sdata_url to be provided")
  expect_error(SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", sdata_url = "http://example.com/test.sdata.zarr"), 
               "Expected one of sdata_path or sdata_url to be provided")
})

test_that("SpatialDataWrapper creates remote instance with sdata_url", {
  w <- SpatialDataWrapper$new(sdata_url = "http://example.com/test.sdata.zarr")
  
  expect_equal(w$sdata_url, "http://example.com/test.sdata.zarr")
  expect_true(w$is_remote)
})

test_that("SpatialDataWrapper get_zarr_url works for remote", {
  w <- SpatialDataWrapper$new(sdata_url = "http://example.com/test.sdata.zarr")
  
  url <- w$get_zarr_url("http://localhost:8000", "dataset1", 1)
  expect_equal(url, "http://example.com/test.sdata.zarr")
})

test_that("SpatialDataWrapper get_zarr_url works for local", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr")
  
  url <- w$get_zarr_url("http://localhost:8000", "dataset1", 1)
  expected_url <- paste0("http://localhost:8000/dataset1/1/", w$local_dir_uid)
  expect_equal(url, expected_url)
})

test_that("SpatialDataWrapper can be created with all optional parameters", {
  w <- SpatialDataWrapper$new(
    sdata_path = "test.sdata.zarr",
    image_path = "images/image1",
    region = "region1", 
    coordinate_system = "global",
    obs_spots_path = "shapes/spots",
    obs_segmentations_path = "labels/segmentation",
    table_path = "tables/custom_table",
    is_zip = TRUE,
    coordination_values = list(obsType = "spot")
  )
  
  expect_equal(w$image_path, "images/image1")
  expect_equal(w$region, "region1")
  expect_equal(w$coordinate_system, "global")
  expect_equal(w$obs_spots_path, "shapes/spots")
  expect_equal(w$obs_segmentations_path, "labels/segmentation")
  expect_equal(w$table_path, "tables/custom_table")
  expect_true(w$is_zip)
})

test_that("SpatialDataWrapper gen_sdata_image_schema works", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", image_path = "images/image1", coordinate_system = "global")
  
  options <- obj_list()
  options <- w$gen_sdata_image_schema(options)
  
  expect_equal(options$image$path, "images/image1")
  expect_equal(options$image$coordinateSystem, "global")
})

test_that("SpatialDataWrapper gen_sdata_obs_spots_schema works", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", 
                              obs_spots_path = "shapes/spots", 
                              table_path = "tables/table",
                              region = "region1",
                              coordinate_system = "global")
  
  options <- obj_list()
  options <- w$gen_sdata_obs_spots_schema(options)
  
  expect_equal(options$obsSpots$path, "shapes/spots")
  expect_equal(options$obsSpots$tablePath, "tables/table")
  expect_equal(options$obsSpots$region, "region1")
  expect_equal(options$obsSpots$coordinateSystem, "global")
})

test_that("SpatialDataWrapper gen_sdata_obs_segmentations_schema works", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", 
                              obs_segmentations_path = "labels/segmentation",
                              table_path = "tables/table",
                              coordinate_system = "global")
  
  options <- obj_list()
  options <- w$gen_sdata_obs_segmentations_schema(options)
  
  expect_equal(options$obsSegmentations$path, "labels/segmentation")
  expect_equal(options$obsSegmentations$tablePath, "tables/table")
  expect_equal(options$obsSegmentations$coordinateSystem, "global")
})

test_that("SpatialDataWrapper make_file_def_creator creates correct file definition", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", image_path = "images/image1")
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "spatialdata.zarr")
  expect_true(grepl("http://localhost:8000/dataset1/1/", file_def$url))
  expect_equal(file_def$options$image$path, "images/image1")
})

test_that("SpatialDataWrapper make_file_def_creator creates correct zip file definition", {
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", image_path = "images/image1", is_zip = TRUE)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "spatialdata.zarr.zip")
})

test_that("SpatialDataWrapper make_file_def_creator includes coordination_values when provided", {
  coord_values <- list(obsType = "spot")
  w <- SpatialDataWrapper$new(sdata_path = "test.sdata.zarr", coordination_values = coord_values)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$coordinationValues, coord_values)
})