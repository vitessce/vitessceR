# Reference: https://rstudio.github.io/reticulate/articles/package.html
# global reference to zarr (will be initialized in .onLoad)
zarr <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to zarr
  zarr <<- reticulate::import("zarr", delay_load = TRUE)
}
