mkdir -p inst/testdata

# Download the test AnnData file
curl -L -o inst/testdata/pbmc3k_filtered_gene_bc_matrices.tar.gz https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz
tar -xvzf inst/testdata/pbmc3k_filtered_gene_bc_matrices.tar.gz -C inst/testdata