
#' Save a Seurat object to an AnnData-Zarr store.
#'
#' @keywords internal
#' @param seurat_obj The object to save.
#' @param out_path A path to the output Zarr store.
#' @param assay The name of the assay to save.
#' @return TRUE if the conversion succeeds.
#'
#' @export
#' @examples
#' obj <- get_seurat_obj()
#' seurat_to_anndata_zarr(obj, out_path = "data/seurat.zarr", assay = "RNA")
seurat_to_anndata_zarr <- function(seurat_obj, out_path, assay) {
  if(!requireNamespace("SeuratDisk", quietly = TRUE)) {
    stop("Install 'SeuratDisk' to enable conversion of Seurat objects to AnnData objects.")
  }

  h5seurat_path <- paste0(out_path, ".h5Seurat")
  h5ad_path <- paste0(out_path, ".h5ad")

  # Convert factor columns to string/numeric.
  seurat_obj@meta.data <- varhandle::unfactor(seurat_obj@meta.data)

  SeuratDisk::SaveH5Seurat(seurat_obj, filename = h5seurat_path, overwrite = TRUE)
  SeuratDisk::Convert(h5seurat_path, dest = "h5ad", overwrite = TRUE, assay = assay)

  # Use basilisk
  proc <- basilisk::basiliskStart(py_env)
  on.exit(basilisk::basiliskStop(proc))

  success <- basilisk::basiliskRun(proc, function(h5ad_path, out_path) {
    anndata <- reticulate::import("anndata")
    zarr <- reticulate::import("zarr")

    adata <- anndata$read_h5ad(h5ad_path)

    cleanup_colnames <- function(df) {
      # Reference: https://github.com/theislab/scvelo/issues/255#issuecomment-739995301
      new_colnames <- colnames(df)
      new_colnames[new_colnames == "_index"] <- "features"
      return(new_colnames)
    }

    noop <- function(cond) { }

    tryCatch({
      colnames(adata$var) <- cleanup_colnames(adata$var)
    }, error = noop)

    # Reconstruct, omitting raw and uns.
    adata <- anndata$AnnData(
      X = adata$X,
      obs = as.data.frame(adata$obs),
      var = as.data.frame(adata$var),
      obsm = adata$obsm,
      varm = adata$varm
    )

    adata$write_zarr(out_path)

    return(TRUE)
  }, h5ad_path = h5ad_path, out_path = out_path)
  return(success)
}

#' Save a SingleCellExperiment to an AnnData-Zarr store.
#'
#' @keywords internal
#' @param sce_obj The object to save.
#' @param out_path A path to the output Zarr store.
#' @return TRUE if the conversion succeeds.
#'
#' @export
#' @examples
#' obj <- get_sce_obj()
#' sce_to_anndata_zarr(obj, out_path = "data/sce.zarr")
#' @importFrom SingleCellExperiment reducedDims reducedDims<-
sce_to_anndata_zarr <- function(sce_obj, out_path) {
  obsm_keys <- names(as.list(reducedDims(sce_obj)))
  for(obsm_key in obsm_keys) {
    # If there are column names, then the obsm element will be stored as a data.frame,
    # but Vitessce can only handle array obsm, so we need to remove any column names.
    # Reference: https://github.com/theislab/zellkonverter/blob/e1e95b1/R/SCE2AnnData.R#L159
    colnames(reducedDims(sce_obj)[[obsm_key]]) <- NULL
  }

  # Use basilisk
  proc <- basilisk::basiliskStart(py_env)
  on.exit(basilisk::basiliskStop(proc))

  success <- basilisk::basiliskRun(proc, function(sce_obj, out_path) {
    anndata <- reticulate::import("anndata")
    zarr <- reticulate::import("zarr")

    adata <- zellkonverter::SCE2AnnData(sce_obj)
    adata$write_zarr(out_path)
    return(TRUE)
  }, sce_obj = sce_obj, out_path = out_path)
  return(success)
}

#' Save a SpatialExperiment to an AnnData-Zarr store.
#'
#' @keywords internal
#' @param spe_obj The object to save.
#' @param out_path A path to the output Zarr store.
#' @return TRUE if the conversion succeeds.
#'
#' @export
#' @examples
#' obj <- get_spe_obj()
#' spe_to_anndata_zarr(obj, out_path = "data/spe.zarr")
#' @importFrom SummarizedExperiment colData
#' @importFrom SingleCellExperiment int_colData
#' @importFrom SpatialExperiment colData<-
spe_to_anndata_zarr <- function(spe_obj, out_path) {
  internal_col_data <- int_colData(spe_obj)

  colData(spe_obj) <- cbind(
    colData(spe_obj),
    internal_col_data$spatialCoords,
    internal_col_data$spatialData,
    internal_col_data$reducedDims
  )

  success <- sce_to_anndata_zarr(spe_obj, out_path)
  return(success)
}

#' Save an image in a SpatialExperiment to an OME-Zarr store
#'
#' @keywords internal
#' @param spe_obj The object containing the image.
#' @param sample_id The sample_id in the imgData data frame.
#' @param image_id The image_id in the imgData data frame.
#' @param out_path A path to the output Zarr store.
#' @return TRUE if the conversion succeeds.
#'
#' @export
#' @examples
#' obj <- get_spe_obj()
#' spe_to_ome_zarr(obj, "sample1", "image1", "data/spe_image.zarr")
#' @importFrom SpatialExperiment getImg
#' @importFrom grDevices as.raster col2rgb
spe_to_ome_zarr <- function(spe_obj, sample_id, image_id, out_path) {
  img_arr <- apply(as.matrix(as.raster(getImg(spe_obj, image_id = image_id, sample_id = sample_id))), c(1, 2), col2rgb)

  # Use basilisk
  proc <- basilisk::basiliskStart(py_env)
  on.exit(basilisk::basiliskStop(proc))

  success <- basilisk::basiliskRun(proc, function(img_arr, sample_id, image_id, out_path) {
    zarr <- reticulate::import("zarr")
    ome_zarr <- reticulate::import("ome_zarr")

    z_root <- zarr$open_group(out_path, mode = "w")

    # Need to copy this here since can't refer to functions in the outside environment.
    obj_list <- function(...) {
      retval <- stats::setNames(list(), character(0))
      param_list <- list(...)
      for(key in names(param_list)) {
        retval[[key]] = param_list[[key]]
      }
      retval
    }

    default_window <- obj_list(
      start = 0,
      min = 0,
      max = 255,
      end = 255
    )

    ome_zarr$writer$write_image(
      image = img_arr,
      group = z_root,
      axes = "cyx",
      omero = obj_list(
        name = image_id,
        version = "0.3",
        rdefs = obj_list(

        ),
        channels = list(
          obj_list(
            label = "r",
            color = "FF0000",
            window = default_window
          ),
          obj_list(
            label = "g",
            color = "00FF00",
            window = default_window
          ),
          obj_list(
            label = "b",
            color = "0000FF",
            window = default_window
          )
        )
      )
    )
    return(TRUE)
  }, img_arr = img_arr, sample_id = sample_id, image_id = image_id, out_path = out_path)
  return(success)
}

#' Save a Giotto object to an AnnData-Zarr store
#'
#' @keywords internal
#' @param giotto_obj The object to save.
#' @param out_path A path to the output Zarr store.
#' @param X_slot The name of the slot in the Giotto object to use for adata.X
#' @return TRUE if the conversion succeeds.
#'
#' @export
#' @examples
#' obj <- get_giotto_obj()
#' giotto_to_anndata_zarr(obj, "data/giotto.zarr")
#' @importFrom methods slot
giotto_to_anndata_zarr <- function(giotto_obj, out_path, X_slot = "raw_exprs") {

  # Use basilisk
  proc <- basilisk::basiliskStart(py_env)
  on.exit(basilisk::basiliskStop(proc))

  success <- basilisk::basiliskRun(proc, function(giotto_obj, out_path, X_slot) {
    anndata <- reticulate::import("anndata")
    zarr <- reticulate::import("zarr")

    # Reference: https://github.com/theislab/zellkonverter/blob/master/R/SCE2AnnData.R#L237
    make_numpy_friendly <- function(x, transpose = TRUE) {
      if (transpose) {
        x <- Matrix::t(x)
      }
      if (DelayedArray::is_sparse(x)) {
        methods::as(x, "dgCMatrix")
      } else {
        as.matrix(x)
      }
    }

    X <- make_numpy_friendly(slot(giotto_obj, X_slot))
    obs <- slot(giotto_obj, "cell_metadata")
    var <- slot(giotto_obj, "gene_metadata")

    adata <- anndata$AnnData(X = X, obs = obs, var = var)

    obsm <- list()

    if(!is.null(slot(giotto_obj, "spatial_locs"))) {
      spatial_locs <- slot(giotto_obj, "spatial_locs")
      obsm[['spatial']] <- t(as.matrix(spatial_locs[, c("sdimx", "sdimy")]))
    }

    if(!is.null(slot(giotto_obj, "dimension_reduction"))) {
      dim_reducs <- slot(giotto_obj, "dimension_reduction")$cells
      for(dim_reduc_name in names(dim_reducs)) {
        dim_reduc_coords <- dim_reducs[[dim_reduc_name]][[dim_reduc_name]]$coordinates
        obsm[[dim_reduc_name]] <- t(as.matrix(dim_reduc_coords))
      }
    }

    if(length(obsm) > 0) {
      # TODO make_numpy_friendly is outside scope
      obsm <- lapply(obsm, make_numpy_friendly)
      adata$obsm <- obsm
    }

    adata$write_zarr(out_path)
    return(TRUE)
  }, giotto_obj = giotto_obj, out_path = out_path, X_slot = X_slot)
  return(success)
}
