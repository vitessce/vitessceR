#' Class representing a local web server static route: path + directory.
#' @keywords internal
#' @rdname VitessceConfigServerStaticRoute
VitessceConfigServerStaticRoute <- R6::R6Class("VitessceConfigServerStaticRoute",
 public = list(
   #' @field path The path on which the web server should respond to requests using this callback.
   path = NULL,
   #' @field directory The directory containing files to serve.
   directory = NULL,
   #' @description
   #' Create a new server route wrapper object.
   #' @param path The route path.
   #' @param directory The directory to serve statically on this route.
   #' @return A new `VitessceConfigServerStaticRoute` object.
   initialize = function(path, directory) {
     self$path <- path
     self$directory <- directory
   }
 )
)

ranged <- function(fp, start = 0, end = NA, block_size = 65535) {
  seek(fp, start)
  data_length <- min(block_size, end - start)
  chunk_data <- readBin(fp, "raw", n = data_length)

  return(chunk_data)
}

#' Class representing a local web server to serve dataset objects.
#' @keywords internal
#' @rdname VitessceConfigServer
VitessceConfigServer <- R6::R6Class("VitessceConfigServer",
  private = list(
    server = NULL,
    port = NULL
  ),
  public = list(
    #' @field num_obj The number of times the on_obj callback has been called.
    num_obj = NULL,
    #' @description
    #' Create a new server wrapper object.
    #' @param port The server port.
    #' @return A new `VitessceConfigServer` object.
    initialize = function(port) {
      cors <- function(res) {
        res$setHeader("Access-Control-Allow-Origin", "*")
        plumber::forward()
      }
      private$server <- plumber::pr()
      private$server <- plumber::pr_set_docs(private$server, FALSE)
      private$server <- plumber::pr_filter(private$server, "CORS", cors)
      private$port <- port
    },
    #' @description
    #' Set up the server routes.
    #' @param routes A list of `VitessceConfigServerStaticRoute` objects.
    create_routes = function(routes) {
      used_paths <- list()
      for(route in routes) {
        if(!(route$path %in% used_paths)) {
          # Reference: https://www.rplumber.io/articles/programmatic-usage.html#mount-static
          #private$server <- plumber::pr_static(private$server, route$path, route$directory)

          private$server <- plumber::pr_handle(private$server, c("GET", "OPTIONS"), route$path, handler = function(req, res) {
            res$setHeader("Access-Control-Allow-Headers", "range")
            res$setHeader("Accept-Ranges", "bytes")

            if(req$REQUEST_METHOD == "GET") {
              file_path <- route$directory

              range_str <- req$HTTP_RANGE
              range_matches <- stringr::str_match(range_str, "^bytes=([:digit:]+)-([:digit:]+)$")
              range_start <- as.numeric(range_matches[2])
              range_end <- as.numeric(range_matches[3])

              file_size <- file.size(file_path)

              res$headers <- obj_list()

              fp <- file("/Users/mkeller/Downloads/exemplar-001.pyramid.ome.tif", "rb")
              seek(fp, range_start)
              data_length <- min(65535, range_end - range_start + 1)
              chunk_data <- readBin(fp, "raw", n = data_length)

              close(fp)

              res$headers[["Content-Range"]] <- paste0("bytes ", as.character(range_start), "-", as.character(range_end), "/", as.character(file_size))
              res$headers[["Content-Length"]] <- as.character(data_length)
              res$headers[["Access-Control-Allow-Origin"]] <- "*"
              res$headers[["Access-Control-Allow-Headers"]] <- "bytes"
              res$headers[["Accept-Ranges"]] <- "bytes"
              res$headers[["Content-Type"]] <- "image/tiff"

              res$headers[["Debug"]] <- paste0(as.character(chunk_data), collapse = "")

              res$body <- chunk_data

              res$status <- 206

            }
            res
          })
          used_paths <- append(used_paths, route$path)
        }
      }
    },
    #' @description
    #' Run the local server on the specified port.
    run = function() {
      plumber::pr_run(private$server, port = private$port)
    }
  )
)
