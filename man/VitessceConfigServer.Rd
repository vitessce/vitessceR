% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{VitessceConfigServer}
\alias{VitessceConfigServer}
\title{Class representing a local web server to serve dataset objects.}
\description{
Class representing a local web server to serve dataset objects.

Class representing a local web server to serve dataset objects.
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{num_obj}}{The number of times the on_obj callback has been called.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-VitessceConfigServer-new}{\code{VitessceConfigServer$new()}}
\item \href{#method-VitessceConfigServer-create_routes}{\code{VitessceConfigServer$create_routes()}}
\item \href{#method-VitessceConfigServer-run}{\code{VitessceConfigServer$run()}}
\item \href{#method-VitessceConfigServer-clone}{\code{VitessceConfigServer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VitessceConfigServer-new"></a>}}
\if{latex}{\out{\hypertarget{method-VitessceConfigServer-new}{}}}
\subsection{Method \code{new()}}{
Create a new server wrapper object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VitessceConfigServer$new(port)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{port}}{The server port.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{VitessceConfigServer} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VitessceConfigServer-create_routes"></a>}}
\if{latex}{\out{\hypertarget{method-VitessceConfigServer-create_routes}{}}}
\subsection{Method \code{create_routes()}}{
Set up the server routes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VitessceConfigServer$create_routes(routes)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{routes}}{A list of \code{VitessceConfigServerStaticRoute} objects.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VitessceConfigServer-run"></a>}}
\if{latex}{\out{\hypertarget{method-VitessceConfigServer-run}{}}}
\subsection{Method \code{run()}}{
Run the local server on the specified port.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VitessceConfigServer$run()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VitessceConfigServer-clone"></a>}}
\if{latex}{\out{\hypertarget{method-VitessceConfigServer-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VitessceConfigServer$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
