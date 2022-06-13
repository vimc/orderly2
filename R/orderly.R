orderly_run <- function(name, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE, echo = TRUE) {
  root <- orderly_root(root, locate)

  src <- file.path(root$path, "src", name)
  dat <- orderly_yml_read(name, src)
  inputs <- c("orderly.yml", dat$script, dat$resources)

  id <- outpack::outpack_id()
  dst <- file.path(root$path, "draft", name, id)
  fs::dir_create(dst)

  ## NOTE: this copy does not cope with nested directories, see
  ## orderly for this working well.
  fs::file_copy(file.path(src, inputs), dst)

  ## TODO: this should have an error handler on it, so that we call
  ## outpack_packet_cancel if anything fails here.
  outpack::outpack_packet_start(dst, name, id = id, root = root$outpack)
  outpack::outpack_packet_file_mark(inputs)
  ## TODO: fairly sure we're not correctly validating the schema here,
  ## try with invalid type for displayname, for example.
  custom <- jsonlite::toJSON(orderly_custom_fields(dat), pretty = FALSE,
                             auto_unbox = FALSE, na = "null", null = "null")
  outpack::outpack_packet_add_custom("orderly2", custom, custom_schema())

  outpack::outpack_packet_run(dat$script, envir %||% .GlobalEnv)
  status <- outpack::outpack_packet_file_list()

  filenames <- unlist(lapply(dat$artefacts, "[[", "filenames"), FALSE, FALSE)
  found <- file_exists(filenames, workdir = dst)
  if (any(!found)) {
    stop("Some files not produced")
  }

  extra <- setdiff(status$path[status$status == "unknown"], filenames)
  if (length(extra) > 0) {
    message("Some extra files found")
  }

  outpack::outpack_packet_end()

  unlink(dst, recursive = TRUE)

  id
}


orderly_custom_fields <- function(dat) {
  custom_artefacts <- lapply(dat$artefacts, function(x)
    list(description = scalar(x$description),
         paths = x$filenames))
  custom_packages <- dat$packages %||% character()
  custom_global <- list()

  ## Not yet handled here: source, global, readme
  custom_role <- data_frame(
    path = c("orderly.yml", dat$script, dat$resources),
    role = c("orderly_yml", "script", rep("resource", length(dat$resources))))

  list(
    artefacts = custom_artefacts,
    packages = custom_packages,
    global = custom_global,
    role = custom_role,
    display_name = dat$display_name,
    description = dat$description,
    custom = NULL)
}


orderly_root <- function(root, locate) {
  ## TODO: we'll need something nice to help here, that will be easier
  ## to do once we know what we actually want to export from the root
  ## I guess.
  if (locate) {
    root <- outpack:::outpack_root_locate(root)
  } else {
    root <- outpack:::outpack_root_open(root)
  }

  if (root$config$core$path_archive == "draft") {
    stop("The option 'core.path_archive' may not be 'draft' for orderly2")
  }

  list(outpack = root, path = root$path)
}
