## Getting Started

The `vitessce` R package has two main parts:

- Widget API: use Vitessce directly from within RStudio
- Config API: create and edit Vitessce view configs using R syntax

## Installation

```r
install.packages("devtools")
devtools::install_github("keller-mark/vitessce-htmlwidgets")
```

## Simplest Widget Use Case

The simplest way to instantiate a Vitessce widget is to create a view config based on a single-cell dataset object (from which data types and visualization types can be inferred automatically) and pass the view config instance as a parameter to the widget function:

```r
library(vitessce)
vc <- vitessce_config_from_object(my_seurat_object)
vitessce_widget(vc)
```

To customize the view config passed into the widget (or to define a view config manually), please see the documentation for the `VitessceConfig` class.