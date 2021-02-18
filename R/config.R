#' Get next scope name
#'
#' @keywords internal
#' @param prev_scopes A vector of previous scope names.
#' @return The new scope name which does not conflict with any previous scope name.
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


#' File in a dataset in a VitessceConfig
#' @title VitessceConfigDatasetFile Class
#' @docType class
#' @description
#' Class representing a file in a dataset in a Vitessce view config.
#'
#' @rdname VitessceConfigDatasetFile
VitessceConfigDatasetFile <- R6::R6Class("VitessceConfigDatasetFile",
  private = list(
    file = NULL
  ),
  public = list(
    #' @description
    #' Create a new dataset file object.
    #' @param url A URL for the file.
    #' @param data_type A data type for the file.
    #' @param file_type A file type for the file.
    #' @return A new `VitessceConfigDatasetFile` object.
    initialize = function(url, data_type, file_type) {
      private$file <- list(
        url = url,
        type = data_type,
        fileType = file_type
      )
    },
    #' @description
    #' Convert the object to an R list. Helpful when converting the config to JSON.
    #' @return A `list` that can be serialized to JSON using `rjson`.
    to_list = function() {
      private$file
    }
  )
)

#' Dataset in a VitessceConfig
#' @title VitessceConfigDataset Class
#' @docType class
#' @description
#' Class representing a dataset in a Vitessce view config.
#'
#' @rdname VitessceConfigDataset
VitessceConfigDataset <- R6::R6Class("VitessceConfigDataset",
  private = list(
    objs = NULL
  ),
  public = list(
    #' @field dataset The dataset as a list.
    dataset = NULL,
    #' @description
    #' Create a new dataset object.
    #' @param uid A unique identifier string for the dataset.
    #' @param name A name for the dataset
    #' @return A new `VitessceConfigDataset` object.
    initialize = function(uid, name) {
      self$dataset <- list(
        uid = uid,
        name = name,
        files = list()
      )
      private$objs <- list()
    },
    #' @description
    #' Add a file to this dataset.
    #' @param url The URL to the file.
    #' @param data_type The data type for the file.
    #' @param file_type The file type for the file.
    #' @return Invisible self, to allow chaining.
    add_file = function(url, data_type, file_type) {
      new_file <- VitessceConfigDatasetFile$new(url, data_type, file_type)
      self$dataset$files <- append(self$dataset$files, new_file)
      invisible(self)
    },
    #' @description
    #' Add a data object to this dataset.
    #' @param obj The data object to add.
    #' @return Invisible self, to allow chaining.
    add_object = function(obj) {
      private$objs <- append(private$objs, obj)
      invisible(self)
    },
    #' @description
    #' Convert the dataset to an R list. Helpful when converting the config to JSON.
    #' @param on_obj An optional function to call upon encountering a dataset object. Used internally by the htmlwidget when rendering local objects.
    #' @return A `list` that can be serialized to JSON using `rjson`.
    to_list = function(on_obj = NA) {
        obj_file_defs <- list()
        if(length(private$objs) > 0) {
          for(i in length(private$objs)) {
            obj <- private$objs[[i]]
            if(is.function(on_obj)) {
              new_obj_file_defs <- on_obj(obj, self$dataset$uid, i)
              for(new_obj_file_def in new_obj_file_defs) {
                obj_file_defs <- append(obj_file_defs, list(new_obj_file_def))
              }
            }
          }
        }

        for(obj_file in self$dataset$files) {
          f_list <- obj_file$to_list()
          obj_file_defs <- append(obj_file_defs, list(f_list))
        }

        retval <- self$dataset
        retval$files <- obj_file_defs
        retval
    }
  )
)

#' Coordination scope in a VitessceConfig
#' @title VitessceConfigCoordinationScope Class
#' @docType class
#' @description
#' Class representing a coordination scope in a Vitessce view config.
#'
#' @rdname VitessceConfigCoordinationScope
VitessceConfigCoordinationScope <- R6::R6Class("VitessceConfigCoordinationScope",
  public = list(
    #' @field c_type The coordination type as a string.
    c_type = NULL,
    #' @field c_scope The coordination scope as a string.
    c_scope = NULL,
    #' @field c_value The coordination value.
    c_value = NULL,
    #' @description
    #' Create a new coordination scope object.
    #' @param c_type A coordination type name.
    #' @param c_scope A coordination scope name.
    #' @return A new `VitessceConfigCoordinationScope` object.
    initialize = function(c_type, c_scope) {
      self$c_type <- c_type
      self$c_scope <- c_scope
      self$c_value <- NA
    },
    #' @description
    #' Set the coordination value of this coordination scope object.
    #' @param c_value The value to set.
    #' @return Invisible self, to allow chaining.
    set_value = function(c_value) {
      self$c_value <- jsonlite::unbox(c_value)
      invisible(self)
    },
    #' @description
    #' Set the coordination value of this coordination scope object, without unboxing.
    #' @param c_value The value to set.
    #' @return Invisible self, to allow chaining.
    set_value_raw = function(c_value) {
      self$c_value <- c_value
      invisible(self)
    }
  )
)

#' Horizontal view concatenation in a VitessceConfig
#' @title VitessceConfigViewHConcat Class
#' @docType class
#' @description
#' Class representing a horizontal view concatenation in a Vitessce view config.
#'
#' @rdname VitessceConfigViewHConcat
#' @keywords internal
VitessceConfigViewHConcat <- R6::R6Class("VitessceConfigViewHConcat",
  public = list(
    #' @field views The views to concatenate.
    views = NULL,
    #' @description
    #' Create a new view concat object.
    #' @param views A list of views.
    #' @return A new `VitessceConfigViewHConcat` object.
    initialize = function(views) {
      self$views <- views
    }
  )
)

#' Vertical view concatenation in a VitessceConfig
#' @title VitessceConfigViewVConcat Class
#' @docType class
#' @description
#' Class representing a vertical view concatenation in a Vitessce view config.
#'
#' @rdname VitessceConfigViewVConcat
#' @keywords internal
VitessceConfigViewVConcat <- R6::R6Class("VitessceConfigViewVConcat",
  public = list(
    #' @field views The views to concatenate.
    views = NULL,
    #' @description
    #' Create a new view concat object.
    #' @param views A list of views.
    #' @return A new `VitessceConfigViewVConcat` object.
    initialize = function(views) {
      self$views <- views
    }
  )
)

#' View in a VitessceConfig
#' @title VitessceConfigView Class
#' @docType class
#' @description
#' Class representing a view in a Vitessce view config.
#'
#' @rdname VitessceConfigView
VitessceConfigView <- R6::R6Class("VitessceConfigView",
  private = list(
    view = NULL
  ),
  public = list(
    #' @description
    #' Create a new view object.
    #' @param component A component name.
    #' @param coordination_scopes A list of the coordination scope mappings for each coordination type.
    #' @param x The x-coordinate of the view in the layout.
    #' @param y The y-coordinate of the view in the layout.
    #' @param w The width of the view in the layout.
    #' @param h The height of the view in the layout.
    #' @return A new `VitessceConfigView` object.
    initialize = function(component, coordination_scopes, x, y, w, h) {
      private$view <- list(
        component = component,
        coordinationScopes = coordination_scopes,
        x = 0,
        y = 0,
        w = 1,
        h = 1
      )
    },
    #' @description
    #' Link this view to existing coordination scope(s).
    #' @param c_scopes A list of `VitessceConfigCoordinationScope` objects to use.
    #' @return Invisible self, to allow chaining.
    use_coordination = function(c_scopes) {
      for(c_scope in c_scopes) {
        private$view$coordinationScopes[[c_scope$c_type]] = c_scope$c_scope
      }
      invisible(self)
    },
    #' @description
    #' Set the dimensions of the view.
    #' @param x The x-coordinate of the view in the layout.
    #' @param y The y-coordinate of the view in the layout.
    #' @param w The width of the view in the layout.
    #' @param h The height of the view in the layout.
    set_xywh = function(x, y, w, h) {
      private$view$x <- x
      private$view$y <- y
      private$view$w <- w
      private$view$h <- h
      invisible(self)
    },
    #' @description
    #' Convert the object to an R list. Helpful when converting the config to JSON.
    #' @return A `list` that can be serialized to JSON using `rjson`.
    to_list = function() {
      private$view
    }
  )
)

#' VitessceConfig
#' @title VitessceConfig Class
#' @docType class
#' @description
#' Class representing a Vitessce view config.
#'
#' @rdname VitessceConfig
#' @export
VitessceConfig <- R6::R6Class("VitessceConfig",
  public = list(
    #' @field config The internal representation of the view config.
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
        coordinationSpace = obj_list(),
        layout = list(),
        initStrategy = "auto"
      )
    },
    #' @description
    #' Add a dataset to the config.
    #' @param name A name for the dataset.
    #' @param uid A unique ID for the dataset. Optional. Created automatically if not provided.
    #' @return A new `VitessceConfigDataset` object.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    add_dataset = function(name, uid = NA) {
      prev_dataset_uids <- c()
      for(d in self$config$datasets) {
        prev_dataset_uids <- c(prev_dataset_uids, d$dataset$uid)
      }
      uid <- ifelse(is.na(uid), get_next_scope(prev_dataset_uids), uid)
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
    #' @param x The x-coordinate of the view in the layout.
    #' @param y The y-coordinate of the view in the layout.
    #' @param w The width of the view in the layout.
    #' @param h The height of the view in the layout.
    #' @param mapping An optional convenience parameter for setting the `CoordinationType$EMBEDDING_TYPE` coordination value if the component is`Component$SCATTERPLOT`.
    #' @return A new `VitessceConfigView` object.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    #' spatial <- vc$add_view(ds, "spatial")
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
        prev_scopes <- character()
        if(!is.null(c_obj)) {
          prev_scopes <- names(c_obj)
        }

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
    #' @returns Self, to allow chaining.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ds <- vc$add_dataset("My dataset")
    #' spatial <- vc$add_view(ds, "spatial")
    #' scatterplot <- vc$add_view(ds, "scatterplot")
    layout = function(view_concat) {
      # TODO

      layout_aux <- function(obj, x_min, x_max, y_min, y_max) {
        w <- x_max - x_min
        h <- y_max - y_min
        if(class(obj)[[1]] == "VitessceConfigView") {
          obj$set_xywh(x_min, y_min, w, h)
        } else if(class(obj)[[1]] == "VitessceConfigViewHConcat") {
          views <- obj$views
          num_views <- length(views)
          for(i in 1:num_views) {
            layout_aux(
              views[[i]],
              x_min+(w/num_views)*(i-1),
              x_min+(w/num_views)*i,
              y_min,
              y_max
            )
          }
        } else if(class(obj)[[1]] == "VitessceConfigViewVConcat") {
          views <- obj$views
          num_views <- length(views)
          for(i in 1:num_views) {
            layout_aux(
              views[[i]],
              x_min,
              x_max,
              y_min+(h/num_views)*(i-1),
              y_min+(h/num_views)*i
            )
          }
        }
      }

      # Recursively set the values (x,y,w,h) for each view.
      layout_aux(view_concat, 0, 12, 0, 12)

      # TODO: decide how to handle views that were omitted from the `view_concat` parameter
      # TODO: decide how to handle .add_view() being called after .layout() has been called

      invisible(self)
    },
    #' @description
    #' A convenience function for setting up new coordination scopes across a set of views.
    #' @param views An array of view objects to link together.
    #' @param c_types The coordination types on which to coordinate the views.
    #' @param c_values Initial values corresponding to each coordination type.
    #' Should have the same length as the c_types array. Optional.
    #' @returns Self, to allow chaining.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' ref_dataset <- vc$add_dataset("Reference")
    #' qry_dataset <- vc$add_dataset("Query")
    #' ref_plot <- vc$add_view(ref_dataset, Component$SCATTERPLOT, mapping = "umap")
    #' qry_plot <- vc$add_view(qry_dataset, Component$SCATTERPLOT, mapping = "proj.umap")
    #' vc$link_views(c(ref_plot, qry_plot), c(CoordinationType$EMBEDDING_TARGET_X, CoordinationType$EMBEDDING_TARGET_Y), c_values = c(0, 0))
    link_views = function(views, c_types, c_values = NA) {
      c_scopes <- self$add_coordination(c_types)
      for(view in views) {
        for(c_scope in c_scopes) {
          view$use_coordination(c(c_scope))
        }
      }
      if(!is.na(c_values) && length(c_types) == length(c_values)) {
        for(i in 1:length(c_scopes)) {
          c_scope <- c_scopes[[i]]
          c_scope$set_value(c_values[[i]])
        }
      }
      invisible(self)
    },
    #' @description
    #' Convert the config to an R list. Helpful when converting the config to JSON.
    #' @param on_obj An optional function to call upon encountering a dataset object. Used internally by the htmlwidget when rendering local objects.
    #' @returns A `list` that can be serialized to JSON using `rjson`.
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

      retval_coordination_space <- obj_list()
      for(c_type in names(self$config$coordinationSpace)) {
        retval_coordination_space[[c_type]] <- obj_list()
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
    },
    #' @description
    #' Create an htmlwidget based on this config.
    #' @param theme The theme of the widget, either "dark" or "light". Optional. By default, "dark".
    #' @param width The width of the widget as a number or CSS string. Optional.
    #' @param height The height of the widget as a number or CSS string. Optional.
    #' @param port The port for the local web server (which serves local dataset objects to the widget).
    #' Optional. By default, uses open port between 8000 and 9000.
    #' @param element_id An element ID. Optional.
    #' @returns The Vitessce htmlwidget.
    #' @examples
    #' vc <- VitessceConfig$new("My config")
    #' dataset <- vc$add_dataset("My dataset")
    #' description <- vc$add_view(dataset, Component$DESCRIPTION)
    #' vc$layout(description)
    #' vc$widget()
    widget = function(...) {
      return(vitessce_widget(config = self, ...))
    }
  )
)


#' @name VitessceConfig$from_list
#' @title Create a new Vitessce Config from a list
#'
#' A helper function to construct a new `VitessceConfig` object based on an existing config in a list format.
#'
#' @param config A list containing a valid config.
#' @return A `VitessceConfig` object reflecting the list-based configuration values.
VitessceConfig$from_list <- function(config) {
  vc <- VitessceConfig$new(config$name, config$description)
  for(d in config$datasets) {
    new_dataset <- vc$add_dataset(d$name, uid = d$uid)
    for(f in d$files) {
      new_dataset$add_file(
        f$url,
        f$type,
        f$fileType
      )
    }
  }
  for(c_type in names(config$coordinationSpace)) {
    if(c_type != CoordinationType$DATASET) {
      c_obj <- config$coordinationSpace[[c_type]]
      vc$config$coordinationSpace[[c_type]] <- obj_list()
      for(c_scope_name in names(c_obj)) {
        c_scope_value <- c_obj[[c_scope_name]]
        scope <- VitessceConfigCoordinationScope$new(c_type, c_scope_name)
        scope$set_value(c_scope_value)
        vc$config$coordinationSpace[[c_type]][[c_scope_name]] <- scope
      }
    }
  }
  for(c in config$layout) {
    new_view <- VitessceConfigView$new(c$component, c$coordinationScopes, c$x, c$y, c$w, c$h)
    vc$config$layout <- append(vc$config$layout, new_view)
  }
  vc
}

#' @name VitessceConfig$from_object
#' @title Create a new Vitessce Config from a data object
#'
#' A helper function to construct a new `VitessceConfig` object based on an object containing single-cell or imaging data.
#'
#' @param obj An object from which to construct a config. Can be a SingleCellExperiment or Seurat object.
#' @param name A name for the view config.
#' @param description A description for the view config.
#' @return A `VitessceConfig` object containing the object as a member of the datasets list, with some automatically-configured views.
VitessceConfig$from_object <- function(obj, name = NA, description = NA) {
  vc <- VitessceConfig$new(name, description)
  ds <- vc$add_dataset("From object")
  ds$add_object(obj)
  # TODO: infer views and coordinations
  vc
}
