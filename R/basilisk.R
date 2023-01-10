#' The Python environment
#'
#' Defines a conda environment via Basilisk, which is used
#' to convert R objects to Zarr stores.
#' This environment has been adapted from zellkonverter::.AnnDataDependencies.
#' Reference: https://bioconductor.org/packages/release/bioc/vignettes/basilisk/inst/doc/motivation.html
#'
#' @keywords internal
py_env <- basilisk::BasiliskEnvironment(
  envname="vitessce_basilisk_env",
  pkgname="vitessceR",
  packages=c(
    "numpy==1.*",
    "pandas==1.*",
    "anndata==0.7.*",
    "h5py==3.*",
    "hdf5==1.*",
    "natsort==7.*",
    "packaging==20.*",
    "scipy==1.*",
    "sqlite==3.*",
    "zarr==2.*",
    "numcodecs==0.*"
  ),
  pip=c(
    "ome-zarr==0.2.1"
  )
)
