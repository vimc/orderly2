cache <- new.env(parent = emptyenv())

custom_metadata_schema <- function(config) {
  if (is.null(cache$custom_metadata_schema)) {
    path <- system.file("outpack-custom.json", package = "orderly2",
                        mustWork = TRUE)
    cache$custom_metadata_schema <- paste(readLines(path), collapse = "\n")
  }
  schema <- cache$custom_metadata_schema

  ## This is pretty ugly, but we rewrite the orderly2 schema to inject
  ## the new bits from plugins. This would be much easier to do if we
  ## could easily manipulate the json directly, but that will require
  ## at least a V8 dependency (which is not terrible as we'll need
  ## that for jsonvalidate anyway if we were doing schema
  ## validation). The other thing to try would be to update
  ## jsonvalidate to allow adding in custom references and don't
  ## interpolate the entire contents of the schema but just the
  ## reference to it, then provide a list of definitions.
  plugins <- Filter(Negate(is.null), lapply(config$plugins, "[[", "schema"))
  if (length(plugins) > 0) {
    schema <- sub('"plugins": \\{\\s+\\}',
                  paste('"plugins":', to_json(plugins, pretty = TRUE)),
                  schema)
  }
  schema
}
