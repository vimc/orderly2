##' Run a report.  This will create a new directory in
##' `drafts/<reportname>`, copy your declared resources there, run
##' your script and check that all expected artefacts were created.
##'
##' @section Differences with orderly:
##'
##' There is no longer a "commit" phase, we will write a vignette
##'   explaining this change more fully.
##'
##' Not supported yet:
##'
##' * automatic handling of README files
##' * dependencies (at least mrc-3113)
##' * global resources (requires more orderly2 configuration)
##' * fields (requires more orderly2 configuration)
##' * tags (we will probably remove these)
##' * environment
##' * secrets
##' * all database support (connection, data and views, plus interpolation
##'   of parameters into queries)
##' * automatic installation of missing packages (VIMC-6626)
##' * check balance of the device and conneciton stack (VIMC-6628)
##' * store random number state in metadata (VIMC-6627)
##' * store git status in metadata (mrc-3380)
##' * save metadata on failure (mrc-3379)
##' * loading environment variables from orderly_envir.yml
##' * echoing run output to console not supported (mrc-3381)
##'
##' Smaller behavioural changes that might be up for changing later
##'
##' * Strip leading `src/` from `name`
##' * Allow `name = NULL` and use current working directory
##' * The `.outpack/` directory now marks the root, not the
##'   `orderly_config.yml`  file, though this must also exist.
##'
##' @title Run a report
##'
##' @param name Name of the report to run
##'
##' @param parameters Parameters passed to the report. A named list of
##'   parameters declared in the `orderly.yml`.  Each parameter
##'   must be a scalar character, numeric, integer or logical.
##'
##' @param envir The environment that will be used to evaluate the
##'   report script; by default we use the global environment, which
##'   may not always be what is wanted.
##'
##' @param root The path to an orderly root directory, or `NULL`
##'   (the default) to search for one from the current working
##'   directory if `locate` is `TRUE`.
##'
##' @param locate Logical, indicating if the configuration should be
##'   searched for.  If `TRUE` and `config` is not given,
##'   then orderly looks in the working directory and up through its
##'   parents until it finds an `.outpack` directory
##'
##' @return The id of the newly created report
##' @export
orderly_run <- function(name, parameters = NULL, envir = NULL,
                        root = NULL, locate = TRUE) {
  root <- orderly_root(root, locate)

  src <- file.path(root$path, "src", name)
  dat <- orderly_yml_read(name, src, root)

  parameters <- check_parameters(parameters, dat$parameters)
  inputs <- c("orderly.yml", dat$script, dat$resources, dat$sources)

  dat$depends <- resolve_dependencies(dat$depends, parameters, root)
  custom_metadata <- to_json(orderly_custom_metadata(dat))

  expected <- unlist(lapply(dat$artefacts, "[[", "filenames"), FALSE, FALSE)

  id <- outpack::outpack_id()
  path <- file.path(root$path, "draft", name, id)
  fs::dir_create(path)

  fs::dir_create(file.path(path, dirname(inputs)))
  ## TODO: I don't think this copes with things within directories?
  fs::file_copy(file.path(src, inputs), path)

  if (!is.null(dat$global_resources)) {
    fs::dir_create(file.path(path, dirname(dat$global_resources$here)))
    fs::file_copy(dat$global_resources$path,
                  file.path(path, dat$global_resources$here))
    inputs <- c(inputs, dat$global_resources$here)
  }

  envir <- envir %||% .GlobalEnv
  assert_is(envir, "environment")

  withCallingHandlers({
    outpack::outpack_packet_start(path, name, parameters = parameters,
                                  id = id, root = root$outpack)
    if (!is.null(parameters)) {
      list2env(parameters, envir)
    }
    outpack::outpack_packet_file_mark(inputs, "immutable")
    outpack::outpack_packet_add_custom("orderly", custom_metadata,
                                       custom_metadata_schema())

    for (p in dat$packages) {
      library(p, character.only = TRUE)
    }
    withr::with_dir(path, {
      for (s in dat$sources) {
        sys.source(s, envir = envir)
      }
    })
    for (i in seq_len(NROW(dat$depends))) {
      outpack::outpack_packet_use_dependency(dat$depends$id[[i]],
                                             dat$depends$files[[i]])
    }

    outpack::outpack_packet_run(dat$script, envir)
    check_produced_files(path, expected, outpack::outpack_packet_file_list())
    outpack::outpack_packet_end()
    unlink(path, recursive = TRUE)
  }, error = function(e) {
    ## Eventually fail nicely here with mrc-3379
    outpack::outpack_packet_cancel()
  })

  id
}


orderly_custom_metadata <- function(orderly_yml_dat) {
  custom_artefacts <- lapply(orderly_yml_dat$artefacts, function(x) {
    list(description = scalar(x$description),
         paths = x$filenames)
  })
  custom_packages <- orderly_yml_dat$packages %||% character()
  if (is.null(orderly_yml_dat$global_resources)) {
    custom_global <- list()
  } else {
    custom_global <- orderly_yml_dat$global_resources[c("here", "there")]
  }

  ## TODO: possibly we should track the queries that resolved
  ## different dependencies. We do this in orderly but have never used
  ## this information. It might be better to add this to outpack
  ## though?

  ## Not yet handled here: readme
  if (is.null(orderly_yml_dat$depends)) {
    depends_files <- NULL
  } else {
    depends_files <- unlist(lapply(orderly_yml_dat$depends$files, names))
  }
  custom_role <- data_frame(
    path = c("orderly.yml",
             orderly_yml_dat$script,
             orderly_yml_dat$global_resources$here,
             orderly_yml_dat$sources,
             orderly_yml_dat$resources,
             depends_files),
    role = c("orderly_yml",
             "script",
             rep_along("global", orderly_yml_dat$global_resources$here),
             rep_along("source", orderly_yml_dat$sources),
             rep_along("resource", orderly_yml_dat$resources),
             rep_along("dependency", depends_files)))

  list(
    artefacts = custom_artefacts,
    packages = custom_packages,
    global = custom_global,
    role = custom_role,
    displayname = scalar(orderly_yml_dat$displayname),
    description = scalar(orderly_yml_dat$description),
    custom = NULL)
}


check_produced_files <- function(path, expected, status) {
  found <- file_exists(expected, workdir = path)
  if (any(!found)) {
    stop("Script did not produce expected artefacts: ",
         paste(squote(expected[!found]), collapse = ", "))
  }
  extra <- setdiff(status$path[status$status == "unknown"], expected)
  if (length(extra) > 0) {
    ## TODO: once we get logging, this would go through that;
    ## currently the format is different to orderly but that's fine.
    ##
    ## TODO: once mrc-3382 is fixed we can mark these to ignore in
    ## outpack
    message("Some extra files found: ",
            paste(squote(extra), collapse = ", "))
  }
}


## outpack just wants a list of parameters - they can be any key-value
## pairs with string keys and "simple" values. However, orderly cares
## about what parameters are provided against a spec in orderly.yml,
## so we validate that here, filling in defaults and checking no
## additional parameters are provided.
check_parameters <- function(parameters, info) {
  if (length(parameters) > 0) {
    assert_named(parameters, unique = TRUE)
  }
  has_default <- names(info)[vlapply(info, function(x) "default" %in% names(x))]
  msg <- setdiff(setdiff(names(info), names(parameters)), has_default)
  if (length(msg) > 0L) {
    stop("Missing parameters: ", paste(squote(msg), collapse = ", "))
  }
  extra <- setdiff(names(parameters), names(info))
  if (length(extra) > 0L) {
    stop("Extra parameters: ", paste(squote(extra), collapse = ", "))
  }
  if (length(info) == 0) {
    return(NULL)
  }

  use_default <- setdiff(has_default, names(parameters))
  parameters[use_default] <- lapply(info[use_default], "[[", "default")
  parameters[names(info)]
}


resolve_dependency <- function(name, query, parameters, root) {
  ## TODO, pass location information through too, and join them into
  ## the scope, but we don't yet really have orderly remotes
  ## (vimc-6666)
  scope <- bquote(name == .(name))
  ## TODO: tell outpack we expect a single value (mrc-3493)
  id <- outpack::outpack_query(query, parameters, scope,
                               require_unpacked = TRUE, root = root$outpack)
  if (is.na(id)) {
    stop(sprintf("Failed to resolve dependency '%s:%s'",
                 name, query), call. = FALSE)
  }
  id
}


resolve_dependencies <- function(depends, parameters, root) {
  if (is.null(depends)) {
    return(NULL)
  }
  depends$query <- depends$id
  for (i in seq_len(nrow(depends))) {
    depends$id[[i]] <- resolve_dependency(depends$name[[i]], depends$query[[i]],
                                          parameters, root)
  }
  depends
}
