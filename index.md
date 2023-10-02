<br/>
Vitessce is a visual integration tool for exploration of spatial single-cell experiments. To learn more about the features of Vitessce, please visit our [core docs](http://vitessce.io).

## Getting Started

The ``vitessceR`` package includes:

* **Vitessce as an htmlwidget**

  * Use Vitessce directly within RStudio as an interactive widget

* **View config API**

  * Create and edit Vitessce configurations using R6 object-oriented syntax

* **Data preparation**

  * Use data conversion functions from the `vitessceAnalysisR` package to process data stored in common single-cell data structures including Seurat objects.


## Installation

Installation requires R 4.0.0 or greater.

```r
install.packages("devtools")
devtools::install_github("vitessce/vitessceR")
```

## Examples

The simplest way to instantiate a Vitessce widget is to create a view config based on a single-cell dataset object and call the `widget` function on the view config instance:

```r
library(vitessceR)

# Create Vitessce view config
vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
dataset <- vc$add_dataset("My dataset")
scatterplot <- vc$add_view(dataset, Component$SCATTERPLOT, mapping = "pca")
status <- vc$add_view(dataset, Component$STATUS)
cell_sets <- vc$add_view(dataset, Component$OBS_SETS)
vc$layout(hconcat(scatterplot, vconcat(status, cell_sets)))

# Render the Vitessce widget
vc$widget()
```

For a full example, visit the [Usage with JSON](articles/web_only/json_remote.html) page.

To customize the view config passed into the widget (or to define a view config manually), please see the documentation for the [`VitessceConfig`](reference/VitessceConfig.html) class.
