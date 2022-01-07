
# Reference: https://bioconductor.org/packages/release/bioc/vignettes/basilisk/inst/doc/motivation.html
# Must include all packages from zellkonverter::.AnnDataDependencies
#' @export
py_env <- basilisk::BasiliskEnvironment(
  envname="vitessce_r_env",
  pkgname="vitessce",
  packages=c(
    "numpy==1.20.2",
    "pandas==1.2.4",
    "anndata==0.7.6",
    "h5py==3.2.1",
    "hdf5==1.10.6",
    "natsort==7.1.1",
    "packaging==20.9",
    "scipy==1.6.3",
    "sqlite==3.35.5",
    "zarr==2.5.0",
    "numcodecs==0.7.2"
  ),
  pip=c(
    "ome-zarr==0.2.1"
  )
)
