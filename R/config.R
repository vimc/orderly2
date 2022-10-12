orderly_config <- function(path) {
  orderly_config_yml_read(path)
}


orderly_config_yml_read <- function(path) {
  filename <- file.path(path, "orderly_config.yml")
  assert_file_exists(basename(filename), workdir = path,
                     name = "Orderly configuration")
  raw <- yaml_read(filename)

  ## This simplifies the validation
  owd <- setwd(path)
  on.exit(setwd(owd))

  if (!is.null(raw)) {
    assert_named(raw)
  }
  plugins <- orderly_config_validate_plugins(raw$plugins, filename)
  check <- c(list(
    global_resources = orderly_config_validate_global_resources),
    lapply(plugins, "[[", "config"))

  ## Same validation strategy as orderly_yml_read
  required <- character()
  optional <- c("plugins", setdiff(names(check), required))

  check_fields(raw, filename, required, optional)

  dat <- list(plugins = plugins)
  for (x in names(check)) {
    dat[[x]] <- check[[x]](raw[[x]], filename)
  }

  dat
}


orderly_config_validate_plugins <- function(plugins, filename) {
  if (is.null(plugins)) {
    return(NULL)
  }
  assert_named(plugins, unique = TRUE, name = sprintf("%s:plugins", filename))
  for (nm in names(plugins)) {
    check_fields(plugins[[nm]], sprintf("%s:plugins:%s", filename, nm),
                 NULL, NULL)
  }

  ## Once we support additional fields in the validation, this call
  ## will get more complicated.
  ret <- list()
  for (nm in names(plugins)) {
    ret[[nm]] <- load_orderly_plugin(nm)
  }
  ret
}


orderly_config_validate_global_resources <- function(global_resources,
                                                     filename) {
  if (!is.null(global_resources)) {
    assert_is_directory(global_resources, name = "Global resource directory")
    global_resources
  }
}
