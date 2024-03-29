% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constants.R
\docType{data}
\name{Component}
\alias{Component}
\title{Enumeration of Component values}
\format{
An object of class \code{list} of length 12.
}
\usage{
Component
}
\description{
The \code{Component} list contains an enumeration of
valid string constant values representing components.
\itemize{
\item \code{SCATTERPLOT}: The \code{scatterplot} component.
\item \code{SPATIAL}: The \code{spatial} component.
\item \code{DESCRIPTION}: The \code{description} component.
\item \code{STATUS}: The \code{status} component.
\item \code{CELL_SETS}: The \code{cellSets} component.
\item \code{HEATMAP}: The \code{heatmap} component.
\item \code{LAYER_CONTROLLER}: The \code{layerController} component.
\item \code{CELL_SET_SIZES}: The \code{cellSetSizes} component.
\item \code{GENES}: The \code{genes} component.
}
}
\examples{
vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
dataset <- vc$add_dataset("My dataset")
description <- vc$add_view(dataset, Component$DESCRIPTION)
vc$layout(description)
vc$widget()
}
\keyword{datasets}
