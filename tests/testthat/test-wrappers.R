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

test_that("file_path_to_url_path", {
  posix_with_slash <- file_path_to_url_path("tests/data/test.snap.mtx")
  expect_equal(posix_with_slash, "/tests/data/test.snap.mtx")

  posix_without_slash <- file_path_to_url_path("tests/data/test.snap.mtx", prepend_slash = FALSE)
  expect_equal(posix_without_slash, "tests/data/test.snap.mtx")

  windows_with_slash <- file_path_to_url_path("tests\\data\\test.snap.mtx")
  expect_equal(windows_with_slash, "/tests/data/test.snap.mtx")

  # TODO: do we need to support this?
  # posix_with_dot_and_slash <- file_path_to_url_path("./tests/data/test.snap.mtx")
  # expect_equal(posix_with_dot_and_slash, "/tests/data/test.snap.mtx")

  # posix_with_dot_without_slash <- file_path_to_url_path("./tests/data/test.snap.mtx", prepend_slash = FALSE)
  # expect_equal(posix_with_dot_without_slash, "tests/data/test.snap.mtx")
})
