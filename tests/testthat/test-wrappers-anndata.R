library(vitessceR)

test_that("AnnDataWrapper", {
  w <- AnnDataWrapper$new(
    adata_path = "tests/data/test.h5ad.zarr",
    obs_set_paths=c('obs/CellType'),
    obs_set_names=c('Cell Type'),
    obs_labels_names=c('Cell Label'),
    obs_labels_paths=c('obs/CellLabel'),
    obs_embedding_paths=c('obsm/X_umap'),
    obs_embedding_names=c('UMAP')
  )
  w$local_dir_uid <- "some_uuid.h5ad.zarr"

  file_def_creator <- w$make_file_def_creator("A", "0")
  file_def <- file_def_creator("http://localhost:8000")

  print(file_def)

  expect_equal(file_def, list(
    fileType = "anndata.zarr",
    url = "http://localhost:8000/A/0/some_uuid.h5ad.zarr",
    options = list(
      obsEmbedding = list(
        obj_list(
            path = "obsm/X_umap",
            embeddingType = "UMAP",
            dims = c(0, 1)
        )
      ),
      obsSets = list(
        obj_list(
            path = "obs/CellType",
            name = "Cell Type"
        )
      ),
      obsLabels = list(
        obj_list(
            path = "obs/CellLabel",
            obsLabelsType = "Cell Label"
        )
      )
    )
  ))
})
