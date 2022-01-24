library(vitessceR)
library(Seurat)

test_that("SeuratWrapper create_cells_list", {
  pbmc.data <- Read10X(data.dir = "seurat/filtered_gene_bc_matrices/hg19")

  pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
  pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")
  pbmc <- subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
  pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)
  pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)
  all.genes <- rownames(pbmc)
  pbmc <- ScaleData(pbmc, features = all.genes)
  pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))
  pbmc <- FindNeighbors(pbmc, dims = 1:10)
  pbmc <- FindClusters(pbmc, resolution = 0.5)

  w <- SeuratWrapper$new(
    pbmc,
    cell_embeddings = c("pca"),
    cell_embedding_names = c("PCA"),
    cell_set_metas = c("seurat_clusters"),
    cell_set_meta_names = c("Clusters"),
    out_dir = file.path("seurat", "out")
  )

  cells_file_def <- w$make_cells_file_def_creator("A", "1")("http://localhost")
  cell_sets_file_def <- w$make_cell_sets_file_def_creator("A", "1")("http://localhost")
  expr_mtx_file_def <- w$make_expression_matrix_file_def_creator("A", "1")("http://localhost")

  expect_equal(cells_file_def, list(
    type = "cells",
    fileType = "anndata-cells.zarr",
    url = "http://localhost/A/1/seurat.zarr",
    options = list(
      mappings = list(
        PCA = list(
          key = "obsm/X_pca",
          dims = c(0, 1)
        )
      )
    )
  ))
  expect_equal(cell_sets_file_def, list(
    type = "cell-sets",
    fileType = "anndata-cell-sets.zarr",
    url = "http://localhost/A/1/seurat.zarr",
    options = list(
      list(
        groupName = "Clusters",
        setName = "obs/seurat_clusters"
      )
    )
  ))
  expect_equal(expr_mtx_file_def, list(
    type = "expression-matrix",
    fileType = "anndata-expression-matrix.zarr",
    url = "http://localhost/A/1/seurat.zarr",
    options = list(
      matrix = "X"
    )
  ))

})
