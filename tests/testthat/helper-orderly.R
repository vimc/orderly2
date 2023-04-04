options(outpack.schema_validate =
          requireNamespace("jsonvalidate", quietly = TRUE) &&
          packageVersion("jsonvalidate") >= "1.4.0")


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  orderly_init(tmp)

  config <- character()

  if ("global" %in% examples) {
    config <- c(config,
                "global_resources: global")
    fs::dir_create(file.path(tmp, "global"))
    fs::file_copy(test_path("examples/minimal/data.csv"),
                  file.path(tmp, "global"))
  }

  if ("plugin" %in% examples) {
    config <- c(config,
                "plugins:",
                "  example.random: ~",
                "example.random:",
                "  distribution:",
                "    normal")
  }

  writeLines(config, file.path(tmp, "orderly_config.yml"))
  fs::dir_create(file.path(tmp, "src"))
  for (i in examples) {
    fs::dir_copy(test_path("examples", i), file.path(tmp, "src"))
  }
  tmp
}


append_lines <- function(new, path) {
  txt <- readLines(path)
  writeLines(c(txt, new), path)
}


json_to_df <- function(x) {
  nms <- lapply(x, names)
  stopifnot(length(unique(nms)) == 1)
  nms <- nms[[1]]
  dat <- lapply(nms, function(nm) sapply(x, "[[", nm))
  names(dat) <- nms
  as.data.frame(dat)
}


hash_file <- function(...) {
  outpack:::hash_file(...)
}


test_path <- function(...) {
  if (basename(getwd()) == "testthat") {
    file.path(...)
  } else {
    testthat::test_path(...)
  }
}


## Expects dependencies to be a list of name and id (or search query)
## e.g.
## list(a = "latest", b = "latest(parameter:x == 2)") # nolint
## By convention will expect report artefact to be "data.rds" and will
## map this to "input.rds" in the script.R
create_random_report <- function(root, name = "data", dependencies = NULL) {
  report_dir <- fs::dir_create(file.path(root, "src", name))
  withr::defer_parent(unlink(report_dir, recursive = TRUE))

  yml <- c(
    "script: script.R",
    "artefacts:",
    "  data:",
    "    description: Random data",
    "    filenames: data.rds"
  )

  if (!is.null(dependencies)) {
    assert_named(dependencies)
    formatted_depends <- do.call(c, lapply(names(dependencies), function(name) {
      id <- dependencies[[name]]
      c(sprintf("  - %s:", name),
        sprintf("      id: %s", id),
                "      use:",
                "        input.rds: data.rds")
    }))
    yml <- c(yml, "depends:", formatted_depends)
    script <- c("x <- readRDS(\"input.rds\")",
                "saveRDS(x + runif(10), \"data.rds\")")
  } else {
    script <- "saveRDS(runif(10), \"data.rds\")"
  }

  writeLines(yml, file.path(report_dir, "orderly.yml"))
  writeLines(script, file.path(report_dir, "script.R"))
  invisible(report_dir)
}
