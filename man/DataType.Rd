% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constants.R
\docType{data}
\name{DataType}
\alias{DataType}
\title{Enumeration of Data Type values}
\format{
An object of class \code{list} of length 6.
}
\usage{
DataType
}
\description{
The \code{DataType} list contains an enumeration of
valid string constant values representing data types
for dataset files.
\itemize{
\item \code{CELLS}: The \code{cells} data type.
\item \code{CELL_SETS}: The \code{cell-sets} data type.
\item \code{EXPRESSION_MATRIX}: The \code{expression-matrix} data type.
\item \code{MOLECULES}: The \code{molecules} data type.
\item \code{NEIGHBORHOODS}: The \code{neighborhoods} data type.
\item \code{RASTER}: The \code{raster} data type.
}
}
\examples{
base_url <- "http://localhost:8000/"
vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
dataset <- vc$add_dataset("My dataset")$add_file(
  url = paste0(base_url, "cells.json"),
  data_type = DataType$CELLS,
  file_type = FileType$CELLS_JSON
)
}
\keyword{datasets}
