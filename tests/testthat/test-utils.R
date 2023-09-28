library(vitessceR)

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
