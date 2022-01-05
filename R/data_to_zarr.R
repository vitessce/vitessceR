
# Reference: https://github.com/theislab/zellkonverter/blob/master/R/SCE2AnnData.R#L237
#' @importClassesFrom Matrix dgCMatrix
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

cleanup_colnames <- function(df) {
  # Reference: https://github.com/theislab/scvelo/issues/255#issuecomment-739995301
  new_colnames <- colnames(df)
  new_colnames[new_colnames == "_index"] <- "features"
  return(new_colnames)
}

seurat_to_anndata_zarr <- function(seurat_obj, out_path, assay) {
  h5seurat_path <- paste0(out_path, ".h5Seurat")
  h5ad_path <- paste0(out_path, ".h5ad")

  # Convert factor columns to string/numeric.
  seurat_obj@meta.data <- varhandle::unfactor(seurat_obj@meta.data)

  SeuratDisk::SaveH5Seurat(seurat_obj, filename = h5seurat_path, overwrite = TRUE)
  SeuratDisk::Convert(h5seurat_path, dest = "h5ad", overwrite = TRUE, assay = assay)

  adata <- anndata$read_h5ad(h5ad_path)

  tryCatch({
    colnames(adata$var) <- cleanup_colnames(adata$var)
  }, error = function(cond) {
    print(cond)
  })
  tryCatch({
    adata$raw <- NULL
  }, error = function(cond) {
    print(cond)
  })

  adata$write_zarr(out_path)
}

sce_to_anndata_zarr <- function(sce_obj, out_path) {
  obsm_keys <- names(as.list(reducedDims(sce_obj)))
  for(obsm_key in obsm_keys) {
    # If there are column names, then the obsm element will be stored as a data.frame,
    # but Vitessce can only handle array obsm, so we need to remove any column names.
    # Reference: https://github.com/theislab/zellkonverter/blob/e1e95b1/R/SCE2AnnData.R#L159
    colnames(reducedDims(sce_obj)[[obsm_key]]) <- NULL
  }
  adata <- zellkonverter::SCE2AnnData(sce_obj)
  adata$write_zarr(out_path)
}

spe_to_anndata_zarr <- function(spe_obj, out_path) {
  internal_col_data <- int_colData(spe_obj)

  colData(spe_obj) <- cbind(colData(spe_obj), internal_col_data$spatialCoords, internal_col_data$spatialData, internal_col_data$reducedDims)

  sce_to_anndata_zarr(spe_obj, out_path)
}

spe_to_ome_zarr <- function(spe_obj, sample_id, image_id, out_path) {
  zarr_path <- out_path

  img_arr <- apply(as.matrix(as.raster(getImg(spe_obj, image_id = image_id, sample_id = sample_id))), c(1, 2), col2rgb)

  z_root <- zarr$open_group(zarr_path, mode = "w")

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
}


giotto_to_anndata_zarr <- function(giotto_obj, out_dir, X_slot = "raw_exprs") {
  zarr_path <- file.path(out_dir, "giotto_obj.zarr")

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
    obsm <- lapply(obsm, make_numpy_friendly)
    adata$obsm <- obsm
  }

  adata$write_zarr(zarr_path)
}
