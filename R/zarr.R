#' FSStore for Zarr
#' @title FSStore Class
#' @docType class
#' @description
#' Class representing a FSStore
#' Adapted from https://github.com/zarr-developers/zarr_implementations/blob/c0bd932/generate_data/js/src/fsstore.js#L7
#'
#' @rdname FSStore
#' @export
FSStore <- R6::R6Class("FSStore",
 public = list(
   #' @field root The path to the root of the store.
   #' @keywords internal
   root = NULL,
   #' @description
   #' Create a new file system store.
   #' @param root The path to the root of the store.
   #' @return A new `FSStore` object.
   initialize = function(root) {
    self$root <- root
    if(!dir.exists(root)) {
      dir.create(root, recursive = TRUE, showWarnings = FALSE)
    }
   },
   #' @description
   #' Get an item from the store.
   #' @param key The item key.
   #' @return The item data in a vector of type raw.
   getItem = function(key) {
    fp <- file.path(self$root, key)
    fp_size <- file.info(fp)$size
    fp_pointer <- file(fp, "rb")
    fp_data <- readBin(fp_pointer, what = "raw", n = fp_size)
    close(fp_pointer)
    return(fp_data)
   },
   #' @description
   #' Set an item in the store.
   #' @param key The item key.
   #' @param value The item value as a vector of type raw.
   setItem = function(key, value) {
    fp <- file.path(self$root, key)
    dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
    fp_pointer <- file(fp, "wb")
    writeBin(value, fp_pointer)
    close(fp_pointer)
   },
   #' @description
   #' Determine whether the store contains an item.
   #' @param key The item key.
   #' @return A boolean value.
   containsItem = function(key) {
     fp <- file.path(self$root, key)
     return(file.exists(fp))
   }
 )
)


#' Convert a JSON-like list to a raw type.
#'
#' @param json_as_list An R list to be converted to JSON.
#' @return The raw value.
json_to_raw <- function(json_as_list) {
  json_str <- jsonlite::toJSON(json_as_list)
  json_raw <- charToRaw(json_str)
  return(json_raw)
}



#' Write an R matrix to a Zarr store (one chunk, no compression).
#'
#' @param matrix The matrix as an R matrix.
#' @param rows A vector of row names.
#' @param cols A vector of column names.
#' @param store The Zarr store.
matrix_to_zarr <- function(matrix, rows, cols, store, compressor = NA) {

  num_rows <- nrow(matrix)
  num_cols <- ncol(matrix)

  raw_matrix <- as.raw(matrix)

  compressor_meta <- jsonlite::unbox(NA)
  if(R6::is.R6(compressor)) {
    raw_matrix <- compressor$encode(raw_matrix)
    compressor_meta <- compressor$get_meta()
  }

  zattrs <- list(
    rows = rows,
    cols = cols
  )
  zarray <- list(
    # TODO: set chunk size to something smaller if multiple chunks
    chunks = c(num_rows, num_cols),
    compressor = compressor_meta,
    dtype = jsonlite::unbox("|u1"),
    fill_value = jsonlite::unbox(0),
    filters = jsonlite::unbox(NA),
    order = jsonlite::unbox("C"),
    shape = c(num_rows, num_cols),
    zarr_format = jsonlite::unbox(2)
  )

  store$setItem(".zattrs", json_to_raw(zattrs))
  store$setItem(".zarray", json_to_raw(zarray))
  # TODO: chunks
  store$setItem("0.0", raw_matrix)
}
