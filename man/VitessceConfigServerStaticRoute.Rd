% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{VitessceConfigServerStaticRoute}
\alias{VitessceConfigServerStaticRoute}
\title{Class representing a local web server static route: path + directory.}
\description{
Class representing a local web server static route: path + directory.

Class representing a local web server static route: path + directory.
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{path}}{The path on which the web server should respond to requests using this callback.}

\item{\code{directory}}{The directory containing files to serve.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{VitessceConfigServerStaticRoute$new()}}
\item \href{#method-clone}{\code{VitessceConfigServerStaticRoute$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new server route wrapper object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VitessceConfigServerStaticRoute$new(path, directory)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{The route path.}

\item{\code{directory}}{The directory to serve statically on this route.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{VitessceConfigServerStaticRoute} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VitessceConfigServerStaticRoute$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}