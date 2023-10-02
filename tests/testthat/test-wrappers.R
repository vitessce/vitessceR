library(vitessceR)

test_that("AbstractWrapper get_url", {
  w <- AbstractWrapper$new()

  route <- w$get_url("http://example.com", "A", 1, "cells")
  expect_equal(route, "http://example.com/A/1/cells")
})

test_that("AbstractWrapper get_route_str", {
  w <- AbstractWrapper$new()

  url <- w$get_route_str("A", 1, "cells")
  expect_equal(url, "/A/1/cells")
})

test_that("AbstractWrapper get_out_dir_route", {
  w <- AbstractWrapper$new(out_dir = "test")

  route <- w$get_out_dir_route("A", 1)
  expect_equal(route$path, "/A/1")
  expect_equal(route$directory, "test/A/1")
})

test_that("AbstractWrapper get_out_dir", {
  w <- AbstractWrapper$new(out_dir = "test")

  out_dir <- w$get_out_dir("A", 1, "cells")
  expect_equal(out_dir, "test/A/1/cells")
})


test_that("AbstractWrapper get_local_dir_route without base_dir", {
  w <- AbstractWrapper$new(out_dir = "test")

  adata_path <- "data/test.h5ad.zarr"
  local_dir_uid <- "some_uuid.h5ad.zarr"

  routes <- w$get_local_dir_route("A", 1, adata_path, local_dir_uid)
  expect_equal(routes[[1]]$path, "/A/1/some_uuid.h5ad.zarr")
  expect_equal(routes[[1]]$directory, "data/test.h5ad.zarr")
})

test_that("AbstractWrapper get_local_dir_route with base_dir", {
  w <- AbstractWrapper$new(out_dir = "test")
  w$base_dir <- "data_base"

  adata_path <- "test.h5ad.zarr"
  local_dir_uid <- "some_uuid.h5ad.zarr"

  routes <- w$get_local_dir_route("A", 1, adata_path, local_dir_uid)
  expect_equal(routes[[1]]$path, "/test.h5ad.zarr")
  expect_equal(routes[[1]]$directory, "data_base/test.h5ad.zarr")
})

test_that("AbstractWrapper get_local_file_route without base_dir", {
  w <- AbstractWrapper$new(out_dir = "test")

  adata_path <- "data/test.csv"
  local_dir_uid <- "some_uuid.csv"

  routes <- w$get_local_file_route("A", 1, adata_path, local_dir_uid)
  expect_equal(routes[[1]]$path, "/A/1/some_uuid.csv")
  expect_equal(routes[[1]]$file_path, "data/test.csv")
})

test_that("AbstractWrapper get_local_file_route with base_dir", {
  w <- AbstractWrapper$new(out_dir = "test")
  w$base_dir <- "data_base"

  adata_path <- "test.csv"
  local_dir_uid <- "some_uuid.csv"

  routes <- w$get_local_file_route("A", 1, adata_path, local_dir_uid)
  expect_equal(routes[[1]]$path, "/test.csv")
  expect_equal(routes[[1]]$file_path, "data_base/test.csv")
})

