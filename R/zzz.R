# Reference: https://rstudio.github.io/reticulate/articles/package.html
# global reference to zarr (will be initialized in .onLoad)
zarr <- NULL
numpy <- NULL
anndata <- NULL
ome_zarr <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to zarr
  zarr <<- reticulate::import("zarr", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  anndata <<- reticulate::import("anndata", delay_load = TRUE)
  ome_zarr <<- reticulate::import("ome_zarr", delay_load = TRUE)
}
