


# Set up reticulate
library(reticulate)
use_condaenv("vitessce-jupyter-examples", conda = "/Users/mkeller/miniconda3/bin/conda", required = TRUE)

# Seurat to Zarr via AnnData
devtools::install_github("mojaveazure/seurat-disk")

## Data dependencies
devtools::install_github('satijalab/seurat-data')
library(SeuratData)
SeuratData::InstallData("pbmc3k")
data("pbmc3k.final")

seurat_to_anndata_zarr(pbmc3k.final, out_dir = "seurat")

# SingleCellExperiment to Zarr via AnnData
BiocManager::install("zellkonverter")

## Data dependencies
BiocManager::install("scRNAseq")
library(scRNAseq)

sce_zeisel <- ZeiselBrainData()

sce_to_anndata_zarr(sce_zeisel, out_dir = "seurat")

# SpatialExperiment to Zarr via AnnData
BiocManager::install("SpatialExperiment")
## Data dependencies
BiocManager::install("STexampleData")
library(STexampleData)

spe <- STexampleData::Visium_mouseCoronal()
spe

spe_to_anndata_zarr(spe, out_dir = "seurat")
spe_to_ome_zarr(spe, out_dir = "seurat")

# Giotto to Zarr via AnnData
devtools::install_github("RubD/Giotto")

## Data dependencies
library(Giotto)
instrs = createGiottoInstructions(
  save_plot = TRUE,
  show_plot = FALSE,
  save_dir = "seurat",
  python_path = "/Users/mkeller/anaconda3/envs/vitessce-jupyter-examples/bin/python"
)
getSpatialDataset(dataset = 'seqfish_SS_cortex', directory = "seurat", method = 'wget')

expr_path = file.path("seurat", "cortex_svz_expression.txt")
loc_path = file.path("seurat", "cortex_svz_centroids_coord.txt")
meta_path = file.path("seurat", "cortex_svz_centroids_annot.txt")

# First, merge location and additional metadata.
SS_locations = data.table::fread(loc_path)
cortex_fields = data.table::fread(meta_path)
SS_loc_annot = data.table::merge.data.table(SS_locations, cortex_fields, by = 'ID')
SS_loc_annot[, ID := factor(ID, levels = paste0('cell_',1:913))]
data.table::setorder(SS_loc_annot, ID)
# Create file with offset information.
my_offset_file = data.table::data.table(field = c(0, 1, 2, 3, 4, 5, 6), x_offset = c(0, 1654.97, 1750.75, 1674.35, 675.5, 2048, 675), y_offset = c(0, 0, 0, 0, -1438.02, -1438.02, 0))
# Create a stitch file.
stitch_file = stitchFieldCoordinates(
  location_file = SS_loc_annot,
  offset_file = my_offset_file,
  cumulate_offset_x = T,
  cumulate_offset_y = F,
  field_col = 'FOV',
  reverse_final_x = F,
  reverse_final_y = T
)
stitch_file = stitch_file[,.(ID, X_final, Y_final)]
my_offset_file = my_offset_file[,.(field, x_offset_final, y_offset_final)]
# Create Giotto object
SS_seqfish <- createGiottoObject(
  raw_exprs = expr_path,
  spatial_locs = stitch_file,
  offset_file = my_offset_file,
  instructions = instrs
)
# Filtering, normalization
SS_seqfish = addCellMetadata(
  SS_seqfish,
  new_metadata = cortex_fields,
  by_column = T,
  column_cell_ID = 'ID'
)
cell_metadata = pDataDT(SS_seqfish)
cortex_cell_ids = cell_metadata[FOV %in% 0:4]$cell_ID
SS_seqfish = subsetGiotto(
  SS_seqfish,
  cell_ids = cortex_cell_ids
)
SS_seqfish <- filterGiotto(
  gobject = SS_seqfish,
  expression_threshold = 1,
  gene_det_in_min_cells = 10,
  min_det_genes_per_cell = 10,
  expression_values = c('raw'),
  verbose = T
)
# Normalize
SS_seqfish <- normalizeGiotto(
  gobject = SS_seqfish,
  scalefactor = 6000,
  verbose = T
)
# Add gene & cell statistics
SS_seqfish <- addStatistics(gobject = SS_seqfish)
# Adjust expression matrix for technical or known variables
SS_seqfish <- adjustGiottoMatrix(
  gobject = SS_seqfish,
  expression_values = c('normalized'),
  batch_columns = NULL,
  covariate_columns = c('nr_genes', 'total_expr'),
  return_gobject = TRUE,
  update_slot = c('custom')
)
SS_seqfish <- calculateHVG(
  gobject = SS_seqfish,
  method = 'cov_loess',
  difference_in_cov = 0.1,
  save_param = list(save_name = '3_a_HVGplot', base_height = 5, base_width = 5)
)
# Select genes based on HVG and gene statistics, both found in gene metadata
gene_metadata = fDataDT(SS_seqfish)
featgenes = gene_metadata[hvg == 'yes' & perc_cells > 4 & mean_expr_det > 0.5]$gene_ID
# Run PCA
SS_seqfish <- runPCA(gobject = SS_seqfish, genes_to_use = featgenes, scale_unit = F, center = T)
SS_seqfish <- runtSNE(SS_seqfish, dimensions_to_use = 1:15)
giotto_to_anndata_zarr(SS_seqfish, out_dir = "seurat")


