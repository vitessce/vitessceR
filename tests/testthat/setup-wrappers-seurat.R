setup({
  url <- "https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz"
  dir.create("seurat", showWarnings = FALSE)
  download.file(url, destfile = "seurat/filtered_gene_bc_matrices.tar.gz")
  untar("seurat/filtered_gene_bc_matrices.tar.gz", exdir = "seurat")
})
teardown({
  unlink("seurat/filtered_gene_bc_matrices.tar.gz")
})
