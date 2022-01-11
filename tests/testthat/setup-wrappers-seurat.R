setup({
  url <- "https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz"
  dir.create("seurat", showWarnings = FALSE)
  dir.create(file.path("seurat", "out"), showWarnings = FALSE)
  download.file(url, destfile = file.path("seurat", "filtered_gene_bc_matrices.tar.gz"))
  untar(file.path("seurat", "filtered_gene_bc_matrices.tar.gz"), exdir = "seurat")
})
teardown({
  unlink(file.path("seurat", "filtered_gene_bc_matrices.tar.gz"))
})
