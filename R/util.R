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
