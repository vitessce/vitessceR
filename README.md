<img src="./img/hexsticker.png" width="230" align="right"/> 

# vitessce-r

![R package version](https://img.shields.io/github/r-package/v/vitessce/vitessceR) [![Vitessce JS dependency version](https://img.shields.io/badge/dynamic/json.svg?url=https%3A%2F%2Fraw.githubusercontent.com%2Fvitessce%2FvitessceR%2Fmain%2Fpackage.json&label=vitessce&query=$.dependencies.vitessce&colorB=blue)](https://github.com/vitessce/vitessce/blob/master/CHANGELOG.md) [![docs](https://img.shields.io/badge/docs-📖-57B4E9.svg)](https://vitessce.github.io/vitessceR/)

R API and htmlwidget facilitating interactive visualization of spatial single-cell data with [Vitessce](https://github.com/vitessce/vitessce).

## Installation

Installation requires R 4.0.0 or greater.

```r
install.packages("devtools")
devtools::install_github("vitessce/vitessceR")
```

## Usage

```r
library(vitessceR)

vc <- VitessceConfig$new()
vc$widget()
```

For full examples, visit the [documentation](https://vitessce.github.io/vitessceR/).

For questions and help with using the package, please open a [discussion](https://github.com/vitessce/vitessceR/discussions).

<img src="./img/screenshot.png" width="600" alt="Screenshot of RStudio">

## Development

```sh
npm install
npm run build
npm run split
```

```r
setwd("path/to/vitessceR")
install.packages("htmlwidgets")
install.packages("devtools")
devtools::install()
devtools::load_all()
```

## Testing

```r
devtools::check()
devtools::test()
devtools::run_examples()
```

## Documentation

```r
install.packages("devtools")
install.packages("pkgdown")
devtools::document()
pkgdown::build_site()
```

Documentation is automatically deployed to GitHub pages with GitHub actions.

## Deployment

Currently, the package is only distributed through GitHub.
In the future, we plan to submit the package to CRAN or Bioconductor.

To increment the package version, update it in [`DESCRIPTION`](https://github.com/vitessce/vitessceR/blob/master/DESCRIPTION#L4).

## Resources

- [htmlwidgets: creating a widget](http://www.htmlwidgets.org/develop_intro.html)
- [r leaflet](https://github.com/rstudio/leaflet)
- [R packages](https://r-pkgs.org/)
- [roxygen2 syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html)
- [R6](https://r6.r-lib.org/index.html)
- [R6 roxygen2 syntax](https://www.tidyverse.org/blog/2019/11/roxygen2-7-0-0/#r6-documentation)
- [plumber: programmatic usage](https://www.rplumber.io/articles/programmatic-usage.html)
- [pkgdown](https://pkgdown.r-lib.org/)
- [S4](http://adv-r.had.co.nz/S4.html)
