library(vitessceR)

test_that("JsonWrapper can be created with required parameters", {
  w <- JsonWrapper$new(json_path = "test.json", data_type = "cells")
  
  expect_equal(w$json_path, "test.json")
  expect_equal(w$data_type, "cells")
  expect_false(w$is_remote)
  expect_true(grepl("\\.json$", w$local_json_uid))
})

test_that("JsonWrapper requires data_type", {
  expect_error(JsonWrapper$new(json_path = "test.json"), "Expected data_type to be provided")
})

test_that("JsonWrapper doesn't allow both json_path and json_url", {
  expect_error(JsonWrapper$new(json_path = "test.json", json_url = "http://example.com/test.json", data_type = "cells"), 
               "Did not expect json_url to be provided with json_path")
})

test_that("JsonWrapper requires either json_path or json_url", {
  expect_error(JsonWrapper$new(data_type = "cells"), "Expected either json_url or json_path to be provided")
})

test_that("JsonWrapper creates remote instance with json_url", {
  w <- JsonWrapper$new(json_url = "http://example.com/test.json", data_type = "cells")
  
  expect_equal(w$json_url, "http://example.com/test.json")
  expect_equal(w$data_type, "cells")
  expect_true(w$is_remote)
})

test_that("JsonWrapper get_json_url works for remote", {
  w <- JsonWrapper$new(json_url = "http://example.com/test.json", data_type = "cells")
  
  url <- w$get_json_url("http://localhost:8000", "dataset1", 1)
  expect_equal(url, "http://example.com/test.json")
})

test_that("JsonWrapper get_json_url works for local", {
  w <- JsonWrapper$new(json_path = "test.json", data_type = "cells")
  
  url <- w$get_json_url("http://localhost:8000", "dataset1", 1)
  expected_url <- paste0("http://localhost:8000/dataset1/1/", w$local_json_uid)
  expect_equal(url, expected_url)
})

test_that("JsonWrapper make_file_def_creator creates correct file definition", {
  w <- JsonWrapper$new(json_path = "test.json", data_type = "cells")
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$fileType, "cells.json")
  expect_true(grepl("http://localhost:8000/dataset1/1/", file_def$url))
  expect_true(grepl("\\.json$", file_def$url))
})

test_that("JsonWrapper make_file_def_creator includes options when provided", {
  options <- list(delimiter = ",", header = TRUE)
  w <- JsonWrapper$new(json_path = "test.json", data_type = "cells", options = options)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$options, options)
})

test_that("JsonWrapper make_file_def_creator includes coordination_values when provided", {
  coord_values <- list(obsType = "cell")
  w <- JsonWrapper$new(json_path = "test.json", data_type = "cells", coordination_values = coord_values)
  
  creator <- w$make_file_def_creator("dataset1", 1)
  file_def <- creator("http://localhost:8000")
  
  expect_equal(file_def$coordinationValues, coord_values)
})