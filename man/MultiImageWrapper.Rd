% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers_images.R
\docType{class}
\name{MultiImageWrapper}
\alias{MultiImageWrapper}
\title{MultiImageWrapper Class}
\description{
Class representing image objects in a Vitessce dataset.
}
\details{
Image wrapper class
}
\keyword{internal}
\section{Super class}{
\code{\link[vitessceR:AbstractWrapper]{vitessceR::AbstractWrapper}} -> \code{MultiImageWrapper}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{image_wrappers}}{The object to wrap.}

\item{\code{use_physical_size_scaling}}{Whether or not to scale the image based on the physical size metadata stored in the file.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MultiImageWrapper-new}{\code{MultiImageWrapper$new()}}
\item \href{#method-MultiImageWrapper-convert_and_save}{\code{MultiImageWrapper$convert_and_save()}}
\item \href{#method-MultiImageWrapper-make_raster_routes}{\code{MultiImageWrapper$make_raster_routes()}}
\item \href{#method-MultiImageWrapper-make_raster_file_def_creator}{\code{MultiImageWrapper$make_raster_file_def_creator()}}
\item \href{#method-MultiImageWrapper-clone}{\code{MultiImageWrapper$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="auto_view_config"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-auto_view_config'><code>vitessceR::AbstractWrapper$auto_view_config()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_file_defs"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_file_defs'><code>vitessceR::AbstractWrapper$get_file_defs()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_local_dir_route"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_local_dir_route'><code>vitessceR::AbstractWrapper$get_local_dir_route()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_local_dir_url"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_local_dir_url'><code>vitessceR::AbstractWrapper$get_local_dir_url()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_local_file_route"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_local_file_route'><code>vitessceR::AbstractWrapper$get_local_file_route()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_local_file_url"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_local_file_url'><code>vitessceR::AbstractWrapper$get_local_file_url()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_out_dir"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_out_dir'><code>vitessceR::AbstractWrapper$get_out_dir()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_out_dir_route"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_out_dir_route'><code>vitessceR::AbstractWrapper$get_out_dir_route()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_route_str"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_route_str'><code>vitessceR::AbstractWrapper$get_route_str()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_routes"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_routes'><code>vitessceR::AbstractWrapper$get_routes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_url"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_url'><code>vitessceR::AbstractWrapper$get_url()</code></a></span></li>
<li><span class="pkg-link" data-pkg="vitessceR" data-topic="AbstractWrapper" data-id="get_url_simple"><a href='../../vitessceR/html/AbstractWrapper.html#method-AbstractWrapper-get_url_simple'><code>vitessceR::AbstractWrapper$get_url_simple()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiImageWrapper-new"></a>}}
\if{latex}{\out{\hypertarget{method-MultiImageWrapper-new}{}}}
\subsection{Method \code{new()}}{
Create a wrapper around multiple image objects.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiImageWrapper$new(image_wrappers, use_physical_size_scaling = FALSE, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{image_wrappers}}{A list of individual image wrapper objects.}

\item{\code{use_physical_size_scaling}}{Whether or not to scale the image based on the physical size metadata stored in the file.}

\item{\code{...}}{Parameters inherited from \code{AbstractWrapper}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{MultiImageWrapper} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiImageWrapper-convert_and_save"></a>}}
\if{latex}{\out{\hypertarget{method-MultiImageWrapper-convert_and_save}{}}}
\subsection{Method \code{convert_and_save()}}{
Create the web server routes and file definition creators.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiImageWrapper$convert_and_save(dataset_uid, obj_i)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataset_uid}}{The ID for this dataset.}

\item{\code{obj_i}}{The index of this data object within the dataset.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiImageWrapper-make_raster_routes"></a>}}
\if{latex}{\out{\hypertarget{method-MultiImageWrapper-make_raster_routes}{}}}
\subsection{Method \code{make_raster_routes()}}{
Create a list representing the image routes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiImageWrapper$make_raster_routes()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list of server route objects.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiImageWrapper-make_raster_file_def_creator"></a>}}
\if{latex}{\out{\hypertarget{method-MultiImageWrapper-make_raster_file_def_creator}{}}}
\subsection{Method \code{make_raster_file_def_creator()}}{
Make the file definition creator function for the raster data type.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiImageWrapper$make_raster_file_def_creator(dataset_uid, obj_i)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dataset_uid}}{The ID for this dataset.}

\item{\code{obj_i}}{The index of this data object within the dataset.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A file definition creator function which takes a \code{base_url} parameter.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MultiImageWrapper-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MultiImageWrapper-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MultiImageWrapper$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
