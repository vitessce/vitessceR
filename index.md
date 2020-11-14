## Getting Started

The `vitessce` R package has two main parts:

- Widget API: use Vitessce directly from within RStudio
- Config API: create and edit Vitessce view configs using R syntax

## Installation

```r
install.packages("devtools")
devtools::install_github("vitessce/vitessce-htmlwidgets")
```

## Simplest Widget Use Case

The simplest way to instantiate a Vitessce widget is to create a view config based on a single-cell dataset object (from which data types and visualization types can be inferred automatically) and pass the view config instance as a parameter to the widget function:

```r
library(vitessce)
vc <- VitessceConfig$from_object(my_seurat_object)
vitessce_widget(vc)
```

To customize the view config passed into the widget (or to define a view config manually), please see the documentation for the `VitessceConfig` class.

## Full Seurat Example

The following 

```r
# Install packages
install.packages("devtools")
devtools::install_github("vitessce/vitessce-htmlwidgets")
devtools::install_github("satijalab/seurat", ref = "release/4.0.0")

# Load packages
library(vitessce)
library(Seurat)

# Download example dataset
url <- "https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz"
dir.create("seurat")
download.file(url, destfile = "seurat/filtered_gene_bc_matrices.tar.gz")
untar("seurat/filtered_gene_bc_matrices.tar.gz", exdir = "seurat")

# Load example dataset
pbmc.data <- Read10X(data.dir = "seurat/filtered_gene_bc_matrices/hg19")

# Process example dataset (run PCA)
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc3k", min.cells = 3, min.features = 200)
pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")
pbmc <- subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)
all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))

# Create Vitessce view config
vc <- VitessceConfig$new("My config")
dataset <- vc$add_dataset("My dataset")$add_object(SeuratWrapper$new(pbmc))
scatterplot <- vc$add_view(dataset, Component$SCATTERPLOT, mapping = "pca")
status <- vc$add_view(dataset, Component$STATUS)
vc$layout(hconcat(scatterplot, status))

# Render the Vitessce widget
vitessce_widget(vc, port = 8001)
```