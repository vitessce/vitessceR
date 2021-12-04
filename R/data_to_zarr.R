
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

seurat_to_anndata_zarr <- function(seurat_obj, out_dir) {
  h5seurat_path <- file.path(out_dir, "seurat_obj.h5Seurat")
  h5ad_path <- file.path(out_dir, "seurat_obj.h5ad")
  zarr_path <- file.path(out_dir, "seurat_obj.zarr")

  SeuratDisk::SaveH5Seurat(pbmc3k.final, filename = h5seurat_path)
  SeuratDisk::Convert(h5seurat_path, dest = "h5ad")

  adata <- anndata$read_h5ad(h5ad_path)
  adata$write_zarr(zarr_path)
}

sce_to_anndata_zarr <- function(sce_obj, out_dir) {
  zarr_path <- file.path(out_dir, "sce_obj.zarr")

  adata <- zellkonverter::SCE2AnnData(sce_obj)
  adata$write_zarr(zarr_path)
}

spe_to_anndata_zarr <- function(spe_obj, out_dir) {
  zarr_path <- file.path(out_dir, "spe_obj.zarr")

  internal_col_data <- int_colData(spe_obj)

  colData(spe_obj) <- cbind(colData(spe_obj), internal_col_data$spatialCoords, internal_col_data$spatialData, internal_col_data$reducedDims)

  adata <- zellkonverter::SCE2AnnData(spe_obj)
  adata$write_zarr(zarr_path)
}

spe_to_ome_zarr <- function(spe_obj, out_dir) {
  out_files <- character()
  img_df <- imgData(spe_obj)
  for(img_idx in 1:nrow(img_df)) {
    sample_id <- img_df[img_idx, "sample_id"]
    image_id <- img_df[img_idx, "image_id"]

    zarr_path <- file.path(out_dir, paste0(sample_id, "_", image_id, ".zarr"))

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
    out_files <- c(out_files, zarr_path)
  }
  return(out_files)
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
