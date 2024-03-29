% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widget.R
\name{vitessce_widget}
\alias{vitessce_widget}
\title{Vitessce Widget}
\usage{
vitessce_widget(
  config,
  theme = "dark",
  width = NULL,
  height = NULL,
  port = NA,
  base_url = NA,
  serve = TRUE,
  element_id = NULL
)
}
\arguments{
\item{config}{A view config as a \code{VitessceConfig} object.}

\item{theme}{The theme of the widget, either "dark" or "light". Optional. By default, "dark".}

\item{width}{The width of the widget as a number or CSS string. Optional.}

\item{height}{The height of the widget as a number or CSS string. Optional.}

\item{port}{The port for the local web server (which serves local dataset objects to the widget).
Optional. By default, uses open port between 8000 and 9000.}

\item{base_url}{The base URL for the web server. Optional.
By default, creates a localhost URL which includes the port.}

\item{serve}{Should local data be served by running a local web server with R plumber? By default, TRUE.}

\item{element_id}{An element ID. Optional.}
}
\value{
The htmlwidget.
}
\description{
This function creates a new Vitessce htmlwidget.
A Vitessce widget is defined by a config which specifies dataset(s),
a layout of views, and linked view connections. A config object can be
passed to the widget using the \code{config} parameter.
}
\details{
We do not recommend calling this function directly. Instead, we
recommend calling the \code{widget()} method on the \code{VitessceConfig}
instance.
}
\examples{
vc <- VitessceConfig$new(schema_version = "1.0.16", name = "My config")
vc$widget()
}
