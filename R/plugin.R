##' Create an ordery plugin. A plugin is typically defined by a
##' package and is used to extend orderly by enabling new
##' functionality, declared in `orderly_config.yml` and `orderly.yml`
##' and affecting the running of reports primarily by creating new
##' objects in the report environment.
##'
##' @title Create an orderly plugin
##'
##' @param config A function to read, check and process the
##'   configuration section in `orderly_config.yml`
##'
##' @param read 
##'
##' @param prepare 
##'
##' @param schema 
##'
##' @return 
##' @author Rich FitzJohn
orderly_plugin <- function(check, read, prepare, schema = NULL) {
  assert_is(check, "function")
  if (!is.null(schema)) {
    assert_file_exists(schema, name = "Schema file")
    schema <- paste(readLines(schema), collapse = "\n")
    class(schema) <- "json"
  }
  ret <- list(check = check,
              read = read,
              prepare = prepare,
              schema = schema)
  class(ret) <- "orderly_plugin"
  ret
}



## Missing here is version information on plugins, and plugins
## requiring orderly versions.

load_orderly_plugin <- function(name) {
  assert_scalar_character(name)
  re <- "^(.+)::(.+)$"
  if (grepl(re, name)) {
    package <- sub(re, "\\1", name)
  } else {
    package <- name
  }
  loadNamespace(package)
  plugin <- .plugins[[name]]
  if (is.null(plugin)) {
    stop(sprintf("Plugin '%s' not found", name))
  }

  plugin
}


.plugins <- new.env(parent = emptyenv())

## TODO: infer the package part
orderly_register_plugin <- function(package, value, name = NULL) {
  assert_scalar_character(package)
  if (is.null(name)) {
    name <- package
  } else {
    assert_scalar_character(name)
    name <- sprintf("%s::%s", package, name)
  }
  assert_is(value, "orderly_plugin")
  .plugins[[name]] <- value
}
