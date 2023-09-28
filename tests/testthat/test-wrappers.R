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
