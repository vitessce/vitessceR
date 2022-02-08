.onLoad <- function(libname, pkgname) {
  tryCatch(
    expr = {
      merge_js()
    },
    error = function(e){ 
      message("Error when running merge_js from .onLoad")
    },
    warning = function(w){
      message("Warning when running merge_js from .onLoad")
    }
  )
}
