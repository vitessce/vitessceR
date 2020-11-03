
get_next_scope <- function(prev_scopes) {
  chars <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  get_next <- function(next_char_indices) {
    r <- c()
    for(char_index in next_char_indices) {
      r <- c(chars[char_index], r)
    }
    increment <- TRUE
    for(i in 1:length(next_char_indices)) {
      next_char_indices[i] <- next_char_indices[i] + 1
      val <- next_char_indices[i]
      if(val > length(chars)) {
        next_char_indices[i] <- 1
      } else {
        increment <- FALSE
        break;
      }
    }
    if(increment) {
      next_char_indices <- c(next_char_indices, 1)
    }
    new_scope <- paste(r, sep = "")
    result <- list(ns = new_scope, nci = next_char_indices)
    result
  }
  next_char_indices = c(1)

  next_scope_result <- get_next(next_char_indices)
  next_scope <- next_scope_result$ns
  next_char_indices <- next_scope_result$nci
  while(is.element(next_scope, prev_scopes)) {
    next_scope_result <- get_next(next_char_indices)
    next_scope <- next_scope_result$ns
    next_char_indices <- next_scope_result$nci
  }
  next_scope
}


#' R6 Class representing a file in a dataset in a Vitessce view config.
#' @rdname R6VitessceConfigDatasetFile
VitessceConfigDatasetFile <- R6::R6Class("VitessceConfigDatasetFile",
  public = list(
    file = NULL,
    initialize = function(url, data_type, file_type) {
      self$file <- list(
        url = url,
        type = data_type,
        fileType = file_type
      )
    },
    to_list = function() {
      self$file
    }
  )
)

#' R6 Class representing a dataset in a Vitessce view config.
#' @rdname R6VitessceConfigDataset
VitessceConfigDataset <- R6::R6Class("VitessceConfigDataset",
  public = list(
    dataset = NULL,
    objs = NULL,
    initialize = function(uid, name) {
      self$dataset <- list(
        uid = uid,
        name = name,
        files = list()
      )
      self$objs <- list()
    },
    add_file = function(url, data_type, file_type) {
      new_file <- VitessceConfigDatasetFile$new(url, data_type, file_type)
      self$dataset$files <- append(self$dataset$files, new_file)
      invisible(self)
    },
    add_object = function(obj) {
      self$objs <- append(self$objs, obj)
      invisible(self)
    },
    to_list = function(on_obj = NA) {
        obj_file_defs <- list()
        if(length(self$objs) > 0) {
          for(i in length(self$objs)) {
            obj <- self$objs[[i]]
            if(!is.na(onObj)) {
              new_obj_file_defs <- on_obj(obj, self$dataset$uid, i)
              obj_file_defs <- append(obj_file_defs, new_obj_file_defs)
            }
          }
        }

        retval <- self$dataset
        retval_files <- list()
        for(f in self$dataset$files) {
          file_list <- f$to_list()
          retval_files <- append(retval_files, list(file_list))
        }
        for(file_list in obj_file_defs) {
          retval_files <- append(retval_files, list(file_list))
        }
        retval$files <- retval_files
        retval
    }
  )
)

#' R6 Class representing a coordination scope in a Vitessce view config.
#' @rdname R6VitessceConfigCoordinationScope
VitessceConfigCoordinationScope <- R6::R6Class("VitessceConfigCoordinationScope",
  public = list(
   c_type = NULL,
   c_scope = NULL,
   c_value = NULL,
   initialize = function(c_type, c_scope) {
     self$c_type <- c_type
     self$c_scope <- c_scope
     self$c_value <- NA
   },
   set_value = function(c_value) {
     self$c_value <- c_value
     invisible(self)
   }
  )
)

#' R6 Class representing a view in a Vitessce view config.
#' @rdname R6VitessceConfigView
VitessceConfigView <- R6::R6Class("VitessceConfigView",
  public = list(
    view = NULL,
    initialize = function(component, coordination_scopes, x, y, w, h) {
      self$view <- list(
        component = component,
        coordinationScopes = coordination_scopes,
        x = 0,
        y = 0,
        w = 1,
        h = 1
      )
    },
    use_coordination = function(c_scopes) {
      for(c_scope in c_scopes) {
        self$view$coordinationScopes[[c_scope$c_type]] = c_scope$c_scope
      }
      invisible(self)
    },
    to_list = function() {
      self$view
    }
  )
)

#' R6 Class representing a Vitessce view config.
#' @rdname R6VitessceConfig
#' @export
VitessceConfig <- R6::R6Class("VitessceConfig",
  public = list(
    config = NULL,
    #' @description
    #' Create a new config object.
    #' @param name A name for the config.
    #' @param description A description for the config.
    #' @return A new `VitessceConfig` object.
    initialize = function(name = NA, description = NA) {
      self$config <- list(
        version = "1.0.0",
        name = ifelse(is.na(name), "", name),
        description = ifelse(is.na(description), "", description),
        datasets = list(),
        coordinationSpace = list(),
        layout = list(),
        initStrategy = "auto"
      )
    },
    #' @description
    #' Add a dataset to the config.
    #' @param name A name for the dataset.
    #' @return A new `VitessceConfigDataset` object.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    add_dataset = function(name) {
      prev_dataset_uids <- c()
      for(d in self$config$datasets) {
        prev_dataset_uids <- c(prev_dataset_uids, d$dataset$uid)
      }
      uid <- get_next_scope(prev_dataset_uids)
      new_dataset <- VitessceConfigDataset$new(uid, name)
      self$config$datasets <- append(self$config$datasets, new_dataset)

      new_scopes <- self$add_coordination(CoordinationType$DATASET)
      new_scopes[[1]]$set_value(uid)
      new_dataset
    },
    #' @description
    #' Add a view to the config.
    #' @param dataset The dataset object to associate with this view.
    #' @param component The name of a component to render in this view.
    #' @return A new `VitessceConfigView` object.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    #' scatterplot <- vc$add_view(ds, "scatterplot")
    add_view = function(dataset, component, x = NA, y = NA, w = NA, h = NA, mapping = NA) {
      dataset_scope_name_matches <- c()
      for(scope_name in names(self$config$coordinationSpace[[CoordinationType$DATASET]])) {
        dataset_scope <- self$config$coordinationSpace[[CoordinationType$DATASET]][[scope_name]]
        if(dataset_scope$c_value == dataset$dataset$uid) {
          dataset_scope_name_matches <- c(dataset_scope_name_matches, scope_name)
        }
      }
      dataset_scope_name <- NA
      if(length(dataset_scope_name_matches) == 1) {
        dataset_scope_name <- dataset_scope_name_matches[1]
      }

      coordination_scopes <- list()
      coordination_scopes[[CoordinationType$DATASET]] <- dataset_scope_name
      new_view <- VitessceConfigView$new(component, coordination_scopes, x, y, w, h)

      if(!is.na(mapping)) {
        et_scopes <- self$add_coordination(CoordinationType$EMBEDDING_TYPE)
        et_scope <- et_scopes[[1]]
        et_scope$set_value(mapping)
        new_view$use_coordination(et_scopes)
      }

      self$config$layout <- append(self$config$layout, new_view)
      new_view
    },
    #' @description
    #' Add a coordination scope to the config, for one or more coordination types.
    #' @param c_types The coordination types for which to create new coordination scopes.
    #' @return A list of new `VitessceConfigCoordinationScope` objects.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    #' spatial <- vc$add_view(ds, "spatial")
    #' c_scopes <- vc$add_coordination(c("spatialZoom", "spatialTargetX"))
    #' spatial$use_coordination(c_scopes)
    add_coordination = function(c_types) {
      result <- list()
      for(c_type in c_types) {
        c_obj <- self$config$coordinationSpace[[c_type]]
        prev_scopes <- ifelse(is.null(c_obj), character(), names(c_obj))
        c_scope <- get_next_scope(prev_scopes)
        scope <- VitessceConfigCoordinationScope$new(c_type, c_scope)
        if(!is.element(c_type, names(self$config$coordinationSpace))) {
          self$config$coordinationSpace[[c_type]] <- list()
        }
        self$config$coordinationSpace[[c_type]][[c_scope]] <- scope
        result <- append(result, scope)
      }
      result
    },
    #' @description
    #' Define the layout of views.
    #' @param view_concat A concatenation of views.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    #' spatial <- vc$add_view(ds, "spatial")
    #' scatterplot <- vc$add_view(ds, "scatterplot")
    #' vc$layout(hconcat(spatial, scatterplot))
    layout = function() {
      # TODO
      invisible(self)
    },
    #' @description
    #' Convert the config to an R list. Helpful when converting the config to JSON.
    #' @param on_obj An optional function to call upon encountering a dataset object. Used internally by the htmlwidget when rendering local objects.
    #' @return A `list` that can be serialized to JSON using `rjson`.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    #' vc_list <- vc$to_list()
    to_list = function(on_obj = NA) {
      retval <- self$config

      retval_datasets <- list()
      for(d in self$config$datasets) {
        dataset_list <- d$to_list(on_obj)
        retval_datasets <- append(retval_datasets, list(dataset_list))
      }
      retval$datasets <- retval_datasets

      retval_coordination_space <- list()
      for(c_type in names(self$config$coordinationSpace)) {
        retval_coordination_space[[c_type]] <- list()
        c_scopes <- self$config$coordinationSpace[[c_type]]
        for(c_scope_name in names(c_scopes)) {
          c_scope <- c_scopes[[c_scope_name]]
          retval_coordination_space[[c_type]][[c_scope_name]] <- c_scope$c_value
        }
      }
      retval$coordinationSpace <- retval_coordination_space

      retval_layout <- list()
      for(v in self$config$layout) {
        view_list <- v$to_list()
        retval_layout <- append(retval_layout, list(view_list))
      }
      retval$layout <- retval_layout

      retval
    }
  )
)
