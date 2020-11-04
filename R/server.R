#' R6 Class representing a local web server to serve dataset objects.
#' @rdname R6VitessceConfigServer
VitessceConfigServer <- R6::R6Class("VitessceConfigServer",
  private = list(
    server = NULL,
    port = NULL
  ),
  public = list(
    #' @description
    #' Create a new server wrapper object.
    #' @param port The server port.
    #' @return A new `VitessceConfigServer` object.
    initialize = function(port) {
      private$server <- plumber::pr()
      private$server <- plumber::pr_set_docs(private$server, FALSE)
      private$port <- port
    },
    #' @description
    #' Callback for a dataset object.
    #' @param obj The dataset object to serve.
    #' @param dataset_uid The dataset UID.
    #' @param obj_i The object index within the dataset.
    on_obj = function(obj, dataset_uid, obj_i) {
      private$server <- plumber::pr_get(private$server, "/test", function(req, res) {
        list(hello = "world")
      })

      retval <- list(
        list(
          url = "http://localhost:8000/test",
          type = "cells",
          fileType = "cells.json"
        )
      )
      retval
    },
    #' @description
    #' Run the local server on the specified port.
    run = function() {
      plumber::pr_run(private$server, port = private$port)
    }
  )
)
