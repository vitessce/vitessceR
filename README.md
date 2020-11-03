# vitessce-htmlwidget

R htmlwidget facilitating interactive visualization of spatial single-cell data with Vitessce.

🚧 work in progress ⚠️


## Installation

```r
install.packages("remotes")
remotes::install_github("keller-mark/vitessce-htmlwidgets")
```

## Usage

```r
library(vitessce)
vc <- vitessceConfig()
vitessceWidget(vc)
```

## Development

```sh
npm install
npm run build
```

```r
setwd("path/to/vitessce-htmlwidget")
install.packages("htmlwidgets")
install.packages("devtools")
devtools::document()
devtools::install()
devtools::load_all()
```

## RTesting

```r
devtools::test()
```

## esources

- [htmlwidgets: creating a widget](http://www.htmlwidgets.org/develop_intro.html)
- [r leaflet](https://github.com/rstudio/leaflet)
- [R6](https://r6.r-lib.org/index.html)
- [R packages](https://r-pkgs.org/)
- [roxygen2 syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html)
