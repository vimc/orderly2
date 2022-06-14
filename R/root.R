## In orderly, there's quite a bit more to read here, coping with the
## configuration (orderly_config.yml) which also marks the root.
##
## In orderly, the key fields in the configuration are:
##
## Things that will be removed
##
## * destination - orderly's internal db, now ignored
## * tags - removed as unused
##
## Things that will be entirely reworked
##
## * remote - configuration of remotes, will get a full change as this
##   has lots of issues as is, and because this interacts with
##   outpack's 'location' support
## * changelog - will get overhauled because it's not clear that it works
##   well at the moment
##
## Things that will probably come in without too much change:
## 
## * fields - custom fields
## * vault - vault configuration
## * global_resources - might be the first bit to come back in?
## * database - lower priority, as only VIMC and everything else must work first
## * minimum_orderly_version - the required version
orderly_root <- function(root, locate) {
  root <- outpack::outpack_root_open(root, locate)
  if (root$config$core$path_archive == "draft") {
    stop("The option 'core.path_archive' may not be 'draft' for orderly2")
  }
  path <- root$path
  config <- orderly_config(path)
  ret <- list(outpack = root, config = config, path = path)
  class(ret) <- "orderly_root"
  ret
}


orderly_init <- function(path) {
  if (file.exists(path)) {
    if (!is_directory(path) || length(dir(path)) > 0) {
      stop("'path', if it already exists, must be an empty directory")
    }
  } else {
    fs::dir_create(path)
  }
  outpack::outpack_init(path)
  file.create(file.path(path, "orderly_config.yml"))
  orderly_root(path, locate = FALSE)
}


orderly_config <- function(path) {
  filename <- file.path(path, "orderly_config.yml")
  assert_file_exists("orderly_config.yml", workdir = path)
  dat <- yaml_read(filename)
  if (length(dat) > 0) {
    stop("No configuration options are currently supported")
  }
  NULL
}
