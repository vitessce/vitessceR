mkdir -p tests-data

# Download the test AnnData file
curl -L -o tests-data/pbmc3k_filtered_gene_bc_matrices.tar.gz https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz
tar -xvzf tests-data/pbmc3k_filtered_gene_bc_matrices.tar.gz -C tests-data