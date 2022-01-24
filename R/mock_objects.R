#' Create a mock Seurat object for tests and examples.
#' @return The object.
#' @keywords internal
#' @export
#' @examples
#' obj <- get_seurat_obj()
#' @importFrom stats rpois
get_seurat_obj <- function() {
  if(!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Install 'Seurat' to enable creation of Seurat objects.")
  }

  ncells <- 100
  u <- matrix(rpois(20000, 5), ncol=ncells)
  cell_names <- paste0("Cell", seq_len(ncells))
  rownames(u) <- paste0("Gene", seq_len(nrow(u)))
  colnames(u) <- cell_names
  metadata.test <- data.frame(
    cluster_id = c(rep(1, 50), rep(2, 50))
  )
  rownames(metadata.test) <- cell_names

  obj <- Seurat::CreateSeuratObject(
    counts = u,
    project = "TESTING",
    assay = "RNA",
    names.field = 2,
    names.delim = "-",
    meta.data = metadata.test
  )

  return(obj)
}

#' Create a mock SCE object for tests and examples.
#' @return The object.
#' @keywords internal
#' @export
#' @examples
#' obj <- get_sce_obj()
get_sce_obj <- function() {
  ncells <- 100
  u <- matrix(rpois(20000, 5), ncol=ncells)
  v <- matrix(rnorm(20000), ncol=ncells)
  obj <- SingleCellExperiment::SingleCellExperiment(assays=list(counts=u, logcounts=v))
  rownames(obj) <- paste0("Gene", seq_len(nrow(v)))
  colnames(obj) <- paste0("Cell", seq_len(ncells))
  return(obj)
}

#' Create a mock SPE object for tests and examples.
#' @return The object.
#' @keywords internal
#' @export
#' @examples
#' obj <- get_spe_obj()
#' @importFrom methods new
#' @importFrom stats rnorm rpois runif
get_spe_obj <- function() {

  # Reference: https://github.com/drighelli/SpatialExperiment/blob/cbf6515/tests/testthat/test_SpatialImage-methods.R#L26
  n <- 10 # number of rows / height
  m <- 20 # number of columns / width
  N <- 3  # number of images in SpE
  .mockRaster <- function(n, m) {
    x <- runif(n*m)
    y <- matrix(x, n, m)
    as.raster(y)
  }
  .mockSPI <- function(n, m) {
    r <- .mockRaster(n, m)
    new("LoadedSpatialImage", image=r)
  }
  l <- replicate(N, .mockSPI(n, m))
  sample_id <- paste0("sample", seq_len(N))
  image_id <- paste0("image", seq_len(N))

  u <- matrix(rpois(3*100, 5), ncol=N)
  v <- matrix(rnorm(3*100), ncol=N)

  cd <- S4Vectors::DataFrame(sample_id)
  df <- S4Vectors::DataFrame(sample_id, image_id, data=I(l), scaleFactor=seq_len(N))
  obj <- SpatialExperiment::SpatialExperiment(assays=list(counts=u, logcounts=v), colData=cd, imgData=df)
  return(obj)
}

#' Create a mock Giotto object for tests and examples.
#' @return The object.
#' @keywords internal
#' @export
#' @examples
#' obj <- get_giotto_obj()
get_giotto_obj <- function() {

  if(!requireNamespace("Giotto", quietly = TRUE)) {
    stop("Install 'Giotto' to enable creation of Giotto objects.")
  }

  expr_path <- system.file("extdata", "giotto", "visium_DG_expr.txt", package = "vitessceR")
  loc_path <- system.file("extdata", "giotto", "visium_DG_locs.txt", package = "vitessceR")

  obj <- Giotto::createGiottoObject(raw_exprs = expr_path, spatial_locs = loc_path)

  return(obj)
}
