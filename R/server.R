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
   },
   #' @description
   #' Add handler functions to the Plumber server object to respond on this route.
   #' @param pr_server The server instance.
   #' @return The modified server instance.
   create_handlers = function(pr_server) {
     # Reference: https://www.rplumber.io/articles/programmatic-usage.html#mount-static
     new_server <- plumber::pr_static(pr_server, self$path, self$directory)

     # Handle Zarr stores for OME-Zarr images.
     head_handler <- function(req, res) {
       res$headers[["Accept-Ranges"]] <- "bytes"
       res$headers[["Access-Control-Allow-Origin"]] <- "*"
       res$headers[["Access-Control-Expose-Headers"]] <- "Access-Control-Allow-Origin, Content-Length, Content-Range, Content-Type, Date, Server, Transfer-Encoding, range"
       res$headers[["Content-Type"]] <- "application/octet-stream"
       res$status <- 200
       res
     }
     new_server <- plumber::pr_head(new_server, paste0(self$path, "/<fname>/.zgroup"), handler = head_handler)
     new_server <- plumber::pr_head(new_server, paste0(self$path, "/<fname>/.zarray"), handler = head_handler)
     new_server <- plumber::pr_head(new_server, paste0(self$path, "/<fname>/<zkey>/.zarray"), handler = head_handler)

     return(new_server)
   }
 )
)

#' Class representing a local web server route for a file which needs to support range requests.
#' @keywords internal
#' @rdname VitessceConfigServerRangeRoute
VitessceConfigServerRangeRoute <- R6::R6Class("VitessceConfigServerRangeRoute",
   public = list(
     #' @field path The path on which the web server should respond to requests using this callback.
     path = NULL,
     #' @field file_path The file to serve.
     file_path = NULL,
     #' @description
     #' Create a new server route wrapper object.
     #' @param path The route path.
     #' @param file_path The file to serve on this route.
     #' @return A new `VitessceConfigServerRangeRoute` object.
     initialize = function(path, file_path) {
       self$path <- path
       self$file_path <- file_path
     },
     #' @description
     #' Add handler functions to the Plumber server object to respond on this route.
     #' @param pr_server The server instance.
     #' @return The modified server instance.
     create_handlers = function(pr_server) {
       # Add the handler for range requests.
       new_server <- plumber::pr_handle(pr_server, c("GET", "OPTIONS"), self$path, handler = function(req, res) {
         if(req$REQUEST_METHOD == "GET") {
           file_path <- self$file_path

           range_str <- req$HTTP_RANGE
           range_matches <- stringr::str_match(range_str, "^bytes=([:digit:]+)-([:digit:]+)$")
           range_start <- as.numeric(range_matches[2])
           range_end <- as.numeric(range_matches[3])

           res$headers <- obj_list()

           fp <- file(file_path, "rb")
           seek(fp, range_start)
           data_length <- (range_end - range_start + 1)

           chunk_data <- readBin(fp, "raw", n = data_length)
           close(fp)

           chunk_length <- length(chunk_data)
           file_size <- file.size(file_path)
           if(range_end > file_size) {
             # Pad with zeros if the range_end went beyond the file size.
             chunk_data <- c(chunk_data, rep(0x00, times = range_end - (range_start + chunk_length) + 1))
           }
           # Set headers.
           res$headers[["Content-Range"]] <- paste0("bytes ", as.character(range_start), "-", as.character(range_end), "/", as.character(file_size))
           res$headers[["Content-Length"]] <- as.character(data_length)
           res$headers[["Access-Control-Allow-Origin"]] <- "*"
           res$headers[["Access-Control-Allow-Headers"]] <- "Range"
           res$headers[["Accept-Ranges"]] <- "bytes"
           res$headers[["Content-Type"]] <- "image/tiff; charset=utf-8"

           res$body <- chunk_data
           res$status <- 206
         } else {
           # This is the OPTIONS request.
           res$setHeader("Access-Control-Allow-Headers", "Range")
           res$status <- 204
         }
         # Return the response object.
         res
       })
       return(new_server)
     }
   )
)

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
          private$server <- route$create_handlers(private$server)
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
