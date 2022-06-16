cache <- new.env(parent = emptyenv())

custom_schema <- function() {
  if (is.null(cache$custom_schema)) {
    path <- system.file("outpack-custom.json", package = "orderly2",
                        mustWork = TRUE)
    cache$custom_schema <- paste(readLines(path), collapse = "\n")
  }
  cache$custom_schema
}
