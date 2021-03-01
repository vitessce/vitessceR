#' Class representing a local web server route: path + callback.
#' @keywords internal
#' @rdname VitessceConfigServerCallbackRoute
VitessceConfigServerCallbackRoute <- R6::R6Class("VitessceConfigServerCallbackRoute",
  public = list(
    #' @field path The path on which the web server should respond to requests using this callback.
    path = NULL,
    #' @field callback The response callback.
    callback = NULL,
    #' @description
    #' Create a new server route wrapper object.
    #' @param path The route path.
    #' @param callback The response callback.
    #' @return A new `VitessceConfigServerCallbackRoute` object.
    initialize = function(path, callback) {
      self$path <- path
      self$callback <- callback
    }
  )
)

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
   #' @param callback The response callback.
   #' @return A new `VitessceConfigServerStaticRoute` object.
   initialize = function(path, directory) {
     self$path <- path
     self$directory <- directory
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
      self$num_obj <- 0
    },
    #' @description
    #' Create the server routes.
    #' @param routes A list of route definition objects
    #' (`VitessceConfigServerCallbackRoute` or `VitessceConfigServerStaticRoute`).
    create_routes = function(routes) {
      used_paths <- list()
      for(route in routes) {
        if(class(route)[1] == "VitessceConfigServerCallbackRoute") {
          private$server <- plumber::pr_get(private$server, route$path, route$callback)
        } else {
          # Reference: https://www.rplumber.io/articles/programmatic-usage.html#mount-static
          if(!(route$path %in% used_paths)) {
            print(route$path)
            print(route$directory)
            private$server <- plumber::pr_static(private$server, "/test", "./test_out")
            used_paths <- append(used_paths, route$path)
          }
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
