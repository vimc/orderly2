test_that("Can read simple configuration", {
  path <- "examples/minimal"
  dat <- orderly_yml_read("minimal", path)

  expect_equal(dat$script, "script.R")
  expect_equal(dat$resources, "data.csv")
  ## TODO: rename filenames -> path, drop format
  expect_equal(dat$artefacts,
               list(list(filenames = "mygraph.png",
                         description = "A graph of things",
                         format = "staticgraph")))
})


test_that("prevent use of 'rm(list = ls())' at top level", {
  path <- test_prepare_orderly_example("minimal")
  path_script <- file.path(path, "src", "minimal", "script.R")
  code <- readLines(path_script)
  writeLines(c("rm(list = ls())", code), path_script)

  expect_error(
    orderly_yml_read("minimal", dirname(path_script)),
    "Do not use 'rm(list = ls())' or similar in your script (script.R:1)",
    fixed = TRUE)
  expect_message(
    orderly_yml_read("minimal", dirname(path_script), TRUE),
    "Do not use 'rm(list = ls())' or similar in your script (script.R:1)",
    fixed = TRUE)
})


test_that("Can read configuration with no resources", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  writeLines(c(
    "script: script.R",
    "artefacts:",
    "  staticgraph:",
    "    description: A graph of things",
    "    filenames: mygraph.png"),
    path_yml)
  dat <- orderly_yml_read("minimal", dirname(path_yml))
  expect_null(dat$resources)
})


test_that("Resources must be relative paths, within directory", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  yml <- readLines(path_yml)
  writeLines(sub("data.csv", "../../data.csv", yml), path_yml)
  file.rename(file.path(path, "src", "minimal", "data.csv"),
              file.path(path, "data.csv"))
  ## TODO: this is the same error as orderly1 but it's not a great error.
  expect_error(orderly_yml_read("minimal", dirname(path_yml)),
               "Declared resources not in right place: ../../data.csv")
})


test_that("README files are not resources", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  append_lines("  - README.md", path_yml)
  file.create(file.path(path, "src", "minimal", "README.md"))
  ## TODO: this is the same error as orderly1 but it's not a great error.
  expect_message(dat <- orderly_yml_read("minimal", dirname(path_yml)),
                 "'README.md' should not be listed as a resource")
  expect_equal(dat$resources, "data.csv")
})


test_that("Directory resources are expanded", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  append_lines("  - dir", path_yml)
  dir.create(file.path(path, "src", "minimal", "dir"))
  file.create(file.path(path, "src", "minimal", "dir", c("a", "b", "c")))
  dat <- orderly_yml_read("minimal", dirname(path_yml))
  expect_setequal(dat$resources, c("data.csv", "dir/a", "dir/b", "dir/c"))
})


test_that("Directory resources with trailing slashes are cleaned", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  append_lines("  - dir/", path_yml)
  dir.create(file.path(path, "src", "minimal", "dir"))
  file.create(file.path(path, "src", "minimal", "dir", c("a", "b", "c")))
  dat <- orderly_yml_read("minimal", dirname(path_yml))
  expect_setequal(dat$resources, c("data.csv", "dir/a", "dir/b", "dir/c"))
})
