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

  ## Same validation strategy as orderly_yml_read
  check <- list(
    global_resources = orderly_config_validate_global_resources)
  required <- character()
  optional <- setdiff(names(check), required)

  check_fields(raw, filename, required, optional)

  dat <- list()
  for (x in names(check)) {
    dat[[x]] <- check[[x]](raw[[x]], filename)
  }

  dat
}


orderly_config_validate_global_resources <- function(global_resources,
                                                     filename) {
  if (!is.null(global_resources)) {
    assert_is_directory(global_resources, name = "Global resource directory")
    global_resources
  }
}
