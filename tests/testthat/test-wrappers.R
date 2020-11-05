library(vitessce)

test_that("AbstractWrapper create_response_json", {
  w <- AbstractWrapper$new()

  response_func <- w$create_response_json(list(hello = "world"))
  res <- response_func(NA, NA)

  expect_equal(res, list(
    hello = "world"
  ))
})

test_that("AbstractWrapper get_route", {
  w <- AbstractWrapper$new()

  route <- w$get_route("A", 1, "cells")
  expect_equal(route, "/A/1/cells")
})

test_that("AbstractWrapper get_url", {
  w <- AbstractWrapper$new()

  url <- w$get_url(8000, "A", 1, "cells")
  expect_equal(url, "http://localhost:8000/A/1/cells")
})
