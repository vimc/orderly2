##' Create an orderly plugin. A plugin is typically defined by a
##' package and is used to extend orderly by enabling new
##' functionality, declared in `orderly_config.yml` and `orderly.yml`
##' and affecting the running of reports primarily by creating new
##' objects in the report environment.  This system is discussed in
##' more detail in `vignette("plugins")`, but will be expanded (likely
##' in breaking ways) soon.
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
##' @param read A function to read, check and process the
##'   configuration section in `orderly.yml`, i.e., for a single
##'   report. It will be passed the data for the plugin section of
##'   `orderly.yml`, the full path to `orderly.yml` and also the
##'   orderly root object.  This last object is not yet exported
##'   normally and so is subject to change! It should return the
##'   processed per-report configuration data, which can then be used
##'   by the `run` function.
##'
##' @param run A function to mutate the report state at runtime. This
##'   is evaluated at a specific point in the process that needs
##'   describing. It can have an effect by mutating the environment
##'   and by creating files in the working directory. It will be
##'   passed five arguments; (1) the data returned from the `read`
##'   function, (2) the root object (as passed to `read`), (3) the
##'   report environment, which your function can read and write, (4)
##'   the parameters as passed to [orderly2::orderly_run], and (5) the
##'   path to the working directory for the report, which your
##'   function can use to write new files into.
##'
##' @param schema Optionally a path to a schema for the metadata
##'   created by this plugin. See `vignette("plugins")` for details.
##'
##' @return An `orderly_plugin` object, though normally this would not
##'   be interacted with by users. Typically, this will be passed
##'   directly to [orderly2::orderly_plugin_register]
##'
##' @export
orderly_plugin <- function(check, read, run, schema = NULL) {
  assert_is(check, "function")
  assert_is(read, "function")
  assert_is(run, "function")
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


.plugins <- new.env(parent = emptyenv())


##' Register a plugin
##'
##' @title Register a plugin
##'
##' @param plugin The plugin
##'
##' @param name The name, typically the package name (later this will
##'   be made optional)
##'
##' @return Nothing, this is called for the side effect of registering
##'   a plugin
##'
##' @export
orderly_plugin_register <- function(plugin, name) {
  assert_scalar_character(name)
  assert_is(plugin, "orderly_plugin")
  .plugins[[name]] <- plugin
}


load_orderly_plugin <- function(name) {
  assert_scalar_character(name)
  if (!(name %in% names(.plugins))) {
    loadNamespace(name)
  }
  plugin <- .plugins[[name]]
  if (is.null(plugin)) {
    stop(sprintf("Plugin '%s' not found", name))
  }

  plugin
}
