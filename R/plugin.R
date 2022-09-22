##' Create an ordery plugin. A plugin is typically defined by a
##' package and is used to extend orderly by enabling new
##' functionality, declared in `orderly_config.yml` and `orderly.yml`
##' and affecting the running of reports primarily by creating new
##' objects in the report environment.
##'
##' @title Create an orderly plugin
##'
##' @param config A function to read, check and process the
##'   configuration section in `orderly_config.yml`. This function
##'   will be passed the deserialised data from the plugin's section
##'   of `orderly_config.yml`, and the full path to that file.  As the
##'   order of loading of plugins is not defined, each plugin must
##'   standalone and should not try and interact with other plugins at
##'   load. It should return a processed copy of the configuration
##'   data, to be passed in as the second argument to `read`.
##'
##' @param read A function to read, check and prcess the configuration
##'   section in `orderly.yml`, i.e., for a single report. It will be
##'   passed the data for the plugin section of `orderly.yml`, the
##'   full path to `orderly.yml` and also the orderly root object.
##'   This last object is not yet exported normally and so is subject
##'   to change! It should return the processed
##'
##' @param run A function to mutate the report state at runtime. This
##'   is evaluated at a specific point in the process that needs
##'   describing. It can have an effect by mutating the environment
##'   and by creating files in the working directory.
##'
##' @param schema Optionally a path to a schema for the metadata
##'   created by this plugin.
##'
##' @return An `orderly_plugin` object, though normally this would not
##'   be called by users.
##'
##' @export
orderly_plugin <- function(check, read, run, schema = NULL) {
  assert_is(check, "function")
  if (!is.null(schema)) {
    assert_file_exists(schema, name = "Schema file")
    schema <- paste(readLines(schema), collapse = "\n")
    class(schema) <- "json"
  }
  ret <- list(check = check,
              read = read,
              run = run,
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
  if (!(name %in% names(.plugins))) {
    loadNamespace(package)
  }
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
