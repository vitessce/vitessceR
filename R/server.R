#' Class representing a local web server route: path + callback.
#' @keywords internal
#' @rdname R6VitessceConfigServerRoute
VitessceConfigServerRoute <- R6::R6Class("VitessceConfigServerRoute",
  public = list(
    #' @field path The path on which the web server should respond to requests using this callback.
    path = NULL,
    #' @field callback The response callback.
    callback = NULL,
    #' @description
    #' Create a new server route wrapper object.
    #' @param path The route path.
    #' @param callback The response callback.
    #' @return A new `VitessceConfigServerRoute` object.
    initialize = function(path, callback) {
      self$path <- path
      self$callback <- callback
    }
  )
)

#' Class representing a local web server to serve dataset objects.
#' @keywords internal
#' @rdname R6VitessceConfigServer
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
    #' Callback for a dataset object.
    #' @param obj The dataset object to serve.
    #' @param dataset_uid The dataset UID.
    #' @param obj_i The object index within the dataset.
    on_obj = function(obj, dataset_uid, obj_i) {
      self$num_obj <- self$num_obj + 1

      all_obj_file_defs <- list()
      all_obj_routes <- list()
      for(data_type in DataType) {
        obj_results <- obj$get_data(data_type, private$port, dataset_uid, obj_i)
        if(is.list(obj_results)) {
          obj_file_defs <- obj_results$file_defs
          obj_routes <- obj_results$routes
          all_obj_file_defs <- c(all_obj_file_defs, obj_file_defs)
          all_obj_routes <- c(all_obj_routes, obj_routes)
        }
      }

      for(obj_route in all_obj_routes) {
        private$server <- plumber::pr_get(private$server, obj_route$path, obj_route$callback)
      }

      retval <- list()
      for(obj_file_def in all_obj_file_defs) {
        retval <- append(retval, list(obj_file_def))
      }

      retval
    },
    #' @description
    #' Run the local server on the specified port.
    run = function() {
      plumber::pr_run(private$server, port = private$port)
    }
  )
)
