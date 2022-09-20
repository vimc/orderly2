`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


vlapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, logical(1), ...)
}


is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}


squote <- function(x) {
  sprintf("'%s'", x)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


to_json <- function(obj) {
  jsonlite::toJSON(obj, pretty = FALSE, auto_unbox = FALSE, na = "null",
                   null = "null", json_verbatim = TRUE)
}


rep_along <- function(x, v) {
  rep_len(x, length(v))
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}
