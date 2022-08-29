## It would be nice, if we're sorting this, to:
##
## * require a few tweaks to how the yml is written as there will be a
##   changes so might as well require a few more
## * fully separate parse from on-disk validation
orderly_yml_read <- function(name, path, root, develop = FALSE) {
  filename <- file.path(path, "orderly.yml")
  assert_is_directory(basename(path), workdir = dirname(path),
                      name = "Report working directory")
  assert_file_exists(basename(filename), workdir = dirname(filename),
                     name = "Orderly configuration")

  raw <- yaml_read(filename)

  ## This simplifies the validation
  owd <- setwd(path)
  on.exit(setwd(owd))

  ## Remaining to implement from orderly:

  ## * fields (custom fields have not been super useful, also configuration)
  ## * tags (we'll probably remove these)
  ## * environment (not sure how widely used this is)
  ## * secrets (not sure how widely used this is, requires configuration)
  ## * connection (db interface, will work through later)
  ## * data (db interface, will work through later)
  ## * views (db interface, will work through later)

  ## TODO: Some of these do on-disk validation (usually that files
  ## exist) - we might be better deferring this until later.
  check <- list(
    script = orderly_yml_validate_script,
    packages = orderly_yml_validate_packages,
    sources = orderly_yml_validate_sources,
    displayname = orderly_yml_validate_displayname,
    description = orderly_yml_validate_description,
    parameters = orderly_yml_validate_parameters,
    resources = orderly_yml_validate_resources,
    depends = orderly_yml_validate_depends,
    artefacts = orderly_yml_validate_artefacts,
    global_resources = orderly_yml_validate_global_resources)

  dat <- list(name = name,
              path = path)

  required <- c("script", "artefacts")
  optional <- setdiff(names(check), required)

  check_fields(raw, filename, required, optional)

  for (x in names(check)) {
    pass_on_develop(
      develop,
      dat[[x]] <- check[[x]](raw[[x]], filename, root))
  }

  ## In orderly there's a little extra processing that happens here:

  ## * process changelog entries
  ## * process readme
  ## * a fair bit of db processing
  ## * ensure that all inputs are unique

  dat
}


pass_on_develop <- function(develop, expr) {
  if (develop) {
    tryCatch(expr, error = function(e) orderly_log("warning", e$message))
  } else {
    force(expr)
  }
}


orderly_yml_validate_script <- function(script, filename, root) {
  assert_scalar_character(script, sprintf("%s:script", filename))
  assert_file_exists(script, name = "Script file")
  exprs <- parse(file = script, keep.source = TRUE)
  for (i in seq_along(exprs)) {
    if (is_global_rm(exprs[[i]])) {
      stop(sprintf(
        "Do not use 'rm(list = ls())' or similar in your script (%s:%s)",
        script, utils::getSrcLocation(exprs[i], "line")),
        call. = FALSE)
    }
  }

  script
}


## Later on we need to expand this to support more options about
## package versions etc.
orderly_yml_validate_packages <- function(packages, filename, root) {
  if (is.null(packages)) {
    return(NULL)
  }
  assert_character(packages, sprintf("%s:packages", filename))
  packages
}


orderly_yml_validate_sources <- function(sources, filename, root) {
  if (is.null(sources)) {
    return()
  }
  assert_character(sources, sprintf("%s:%s", filename, "sources"))
  assert_file_exists(sources, name = "Source file")
  sources
}

orderly_yml_validate_displayname <- function(displayname, filename, root) {
  if (is.null(displayname)) {
    return(NULL)
  }
  assert_scalar_character(displayname, sprintf("%s:displayname", filename))
  displayname
}


orderly_yml_validate_description <- function(description, filename, root) {
  if (is.null(description)) {
    return(NULL)
  }
  assert_scalar_character(description, sprintf("%s:description", filename))
  description
}


orderly_yml_validate_parameters <- function(parameters, filename, root) {
  if (is.null(parameters) || length(parameters) == 0L) {
    return(NULL)
  }

  name <- function(p) {
    sprintf("%s:parameters:%s", filename, p)
  }

  assert_named(parameters, TRUE, sprintf("%s:parameters", filename))
  for (p in names(parameters)) {
    if (!is.null(parameters[[p]])) {
      assert_named(parameters[[p]], name = name(p))
      check_fields(parameters[[p]], name(p), NULL, "default")
    }
  }

  parameters
}


orderly_yml_validate_resources <- function(resources, filename, root) {
  if (is.null(resources)) {
    return(NULL)
  }

  ## make sure that a directory resource does not have a trailing /
  ## There is much more sanitation that could be done here...
  is_dir <- is_directory(resources)
  trailing <- grepl(pattern = "(\\/)$", resources)
  bad_resource <- is_dir & trailing
  if (any(bad_resource)) {
    resources[bad_resource] <- sub("(\\/)$", "", resources[bad_resource])
  }

  assert_character(resources, sprintf("%s:%s", filename, "resouces"))
  assert_file_exists(resources, name = "Resource file")
  err <- resources[!is_within_dir(resources, ".")]
  if (length(err) > 0L) {
    stop("Declared resources not in right place: ",
         paste(err, collapse = ", "))
  }

  i <- grepl("README(|\\.md)$", resources, ignore.case = TRUE)
  if (any(i)) {
    orderly_log(
      "warning",
      sprintf("'%s' should not be listed as a resource", resources[i]))
    resources <- resources[!i]
  }

  ## TODO: what happens here about README files that get expanded,
  ## these are not dealt with well at present!
  if (any(is_dir)) {
    resources <- as.list(resources)
    resources[is_dir] <- lapply(resources[is_dir], function(p)
      file.path(p, dir(p, recursive = TRUE, all.files = TRUE)))
    resources <- unlist(resources)
  }

  resources
}


orderly_yml_validate_depends <- function(depends, filename, root) {
  if (is.null(depends)) {
    return(NULL)
  }

  ## Deal with yaml weirdness:
  if (is.null(names(depends))) {
    depends <- ordered_map_to_list(depends)
  }

  for (i in seq_along(depends)) {
    depends[[i]]$name <- names(depends)[[i]]
    depends[[i]] <- recipe_validate_depend1(depends[[i]], filename)
  }

  ret <- rbind_df(depends)
  rownames(ret) <- NULL
  ret
}


recipe_validate_depend1 <- function(depend, filename) {
  name <- depend$name
  v <- c("id", "use", "name")
  check_fields(depend, sprintf("%s:depends:%s", filename, name), v, NULL)

  assert_character(depend$id, sprintf("%s:depends:%s:id", filename, name))
  assert_named(depend$use, TRUE, sprintf("%s:depends:%s:use", filename, name))
  err <- !vlapply(depend$use, function(x) is.character(x) && length(x) == 1)
  if (any(err)) {
    stop(sprintf("%s:depends:%s:use must all be single strings",
                 filename, name),
         call. = FALSE)
  }
  files <- list_to_character(depend$use)

  data_frame(id = depend$id, name = name, files = I(list(files)))
}


## TODO(VIMC-3519): this copies over all the logic from before but
## there are some things I would like to see handled: we should
## simplify the format, and move format into the body like
##
## artefacts:
##   - format: staticgraph
##     description: A graph of things
##     filenames:
##       - filename.png
##
## Which is simpler and will be easier to edit.
orderly_yml_validate_artefacts <- function(artefacts, filename, root) {
  if (length(artefacts) == 0L) {
    stop("At least one artefact required")
  }

  if (is.character(artefacts)) {
    msg <- c("Your artefacts are misformatted.  You must provide a 'type'",
             "and a description for each, and each logical artefact may",
             "contain multiple files.  For example, you might use",
             "",
             "artefacts:",
             "  - data:",
             "      description: These are data for x, y, z",
             "      filenames:",
             sprintf("        - %s", artefacts),
             "",
             sprintf("other alternatives to 'data' are %s",
                     paste(squote(setdiff(valid_formats(), "data")),
                           collapse = ", ")))
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }

  if (!is.null(names(artefacts)) && length(artefacts) != 1L) {
    if (any(names(artefacts) %in% valid_formats())) {
      artefacts <- utils::tail(artefacts, 3)
      correct <- list(artefacts = lapply(seq_along(artefacts),
                                         function(i) artefacts[i]))
      msg <- c("Your artefacts look incorrectly formatted; they must be",
               "an _ordered map_.  Currently you have something like",
               "",
               indent(yaml::as.yaml(list(artefacts = artefacts)), 4),
               "",
               "but you should reformat that as something like",
               "",
               indent(yaml::as.yaml(correct), 4),
               "",
               "otherwise with duplicate entries with the same report type",
               "your yaml will be invalid (this format is permitted for",
               "single artefacts only)")
      message(paste(msg, collapse = "\n"))
    }
    stop("Expected an ordered map!")
  }

  if (is.null(names(artefacts)) && all(lengths(artefacts) == 1L)) {
    artefacts <- ordered_map_to_list(artefacts)
  }

  assert_named(artefacts, FALSE, "artefacts")

  ## Then this part, which is basically going to become a migration
  ## once we change the format, but which doing here simplifies the
  ## next bit:
  for (i in seq_along(artefacts)) {
    artefacts[[i]]$format <- names(artefacts)[[i]]
    artefacts[[i]]$index <- i
  }
  artefacts <- unname(artefacts)

  res <- lapply(artefacts, orderly_yml_validate_artefact1, filename)

  filenames <- unlist(lapply(res, "[[", "filenames"), FALSE, FALSE)
  dups <- unique(filenames[duplicated(filenames)])
  if (length(dups) > 0L) {
    stop("Duplicate artefact filenames are not allowed: ",
         paste(squote(dups), collapse = ", "))
  }

  if (any(grepl("^README(|.md)$", filenames, ignore.case = TRUE))) {
    stop("README.md should not be listed as an artefact")
  }

  res
}


orderly_yml_validate_artefact1 <- function(artefact, filename) {
  index <- artefact$index

  ## NOTE: this means that we silently ignore the format and index
  ## fields if present, which they probably will not be
  v <- c("filenames", "description", "format", "index")
  check_fields(artefact, sprintf("%s:artefacts[%d]", filename, index), v, NULL)

  assert_scalar_character(
    artefact$description,
    sprintf("%s:artefacts[%d]:description", filename, index))
  err <- !vlapply(artefact$filenames, is.character)
  if (any(err)) {
    stop(sprintf(
      "%s:artefacts[%d]:description must be character (check entry %s)",
      filename, index, paste(which(err), collapse = ", ")))
  }

  if (!(artefact$format %in% valid_formats())) {
    stop(sprintf(
      "Unknown artefact type: '%s' for '%s:artefacts[%d]'; should be one of %s",
      artefact$format, filename, artefact$index,
      paste(squote(valid_formats()), collapse = ", ")),
      call. = FALSE)
  }

  artefact[c("filenames", "description", "format")]
}


orderly_yml_validate_global_resources <- function(global_resources, filename,
                                                  root) {
  if (is.null(global_resources)) {
    return(NULL)
  }

  if (is.null(root$config$global_resources)) {
    stop(paste("'global_resources' is not supported;",
               "please edit orderly_config.yml to enable"),
         call. = FALSE)
  }

  prefix <- sprintf("%s:global_resources", filename)

  assert_named(global_resources, name = prefix)
  for (i in seq_along(global_resources)) {
    assert_scalar_character(
      global_resources[[i]],
      sprintf("%s:%s", prefix, names(global_resources)[[i]]))
  }

  here <- names(global_resources)
  there <- list_to_character(global_resources, named = FALSE)

  global_path <- file.path(root$path, root$config$global_resources)
  assert_file_exists(
    there, check_case = TRUE, workdir = global_path,
    name = sprintf("Global resources in '%s'", global_path))

  ## TODO: this is really easy, we just expand at this point; reuse
  ## same logic (trailing slash check, plus expansion) as
  ## orderly_yml_validate_resources, copying it into it's own function
  if (any(is_directory(file.path(global_path, global_resources)))) {
    stop("global resources cannot yet be directories")
  }

  data_frame(here = here,
             there = there,
             path = file.path(global_path, there))
}


valid_formats <- function() {
  c("staticgraph", "interactivegraph", "data", "report", "interactivehtml")
}


is_global_rm <- function(expr) {
  is.recursive(expr) &&
    identical(expr[[1]], quote(rm)) &&
    is.recursive(expr[[2]]) &&
    identical(expr[[2]][[1]], quote(ls))
}


resource_expand_dir <- function(resources) {
  is_dir <- is_directory(resources)

  if (any(is_dir)) {
    is_trailing <- is_dir & grepl(pattern = "(\\/)$", resources)
    resources[is_trailing] <- sub("(\\/)$", "", resources[is_trailing])

    resources <- as.list(resources)
    resources[is_dir] <- lapply(resources[is_dir], function(p) {
      file.path(p, dir(p, recursive = TRUE, all.files = TRUE, no.. = TRUE))
    })
    resources <- unlist(resources)
  }

  resources
}
