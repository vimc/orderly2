test_that("Can read simple configuration", {
  path <- "examples/minimal"
  dat <- orderly_yml_read("minimal", path)

  expect_equal(dat$script, "script.R")
  expect_equal(dat$resources, "data.csv")
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


test_that("At least one artefact required", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  yaml_write(list(script = "script.R",
                  resources = "data.csv",
                  artefacts = NULL),
             path_yml)
  expect_error(orderly_yml_read("minimal", dirname(path_yml)),
               "At least one artefact required")
})


test_that("Helpful error if given strings", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  yaml_write(list(script = "script.R",
                  resources = "data.csv",
                  artefacts = c("a", "b")),
             path_yml)
  err <- expect_error(orderly_yml_read("minimal", dirname(path_yml)),
                      "Your artefacts are misformatted.")
})


test_that("Allow ordered map if single artefact given", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  cmp <- orderly_yml_read("minimal", dirname(path_yml))

  yaml_write(list(script = cmp$script,
                  resources = cmp$resources,
                  artefacts = list(list(
                    staticgraph = list(
                      description = cmp$artefacts[[1]]$description,
                      filenames = cmp$artefacts[[1]]$filenames)))),
             path_yml)
  dat <- orderly_yml_read("minimal", dirname(path_yml))
  expect_identical(dat, cmp)
})


test_that("Filenames must be a character vector", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  writeLines(c(
    "script: script.R",
    "artefacts:",
    "  graph:",
    "    description: A graph of things",
    "    filenames: mygraph.png"),
    path_yml)
  expect_error(
    orderly_yml_read("minimal", dirname(path_yml)),
    "orderly.yml:artefacts[1]'; should be one of 'staticgraph'",
    fixed = TRUE)
})


test_that("Require ordered map with more than one", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  writeLines(c(
    "script: script.R",
    "artefacts:",
    "  staticgraph:",
    "    description: A graph of things",
    "    filenames: mygraph.png",
    "  data:",
    "    description: Some data",
    "    filenames: data.csv"),
    path_yml)

  ## TODO: there are complicated extra tests for verifying that the
  ## provided fix works
  expect_error(
    suppressMessages(orderly_yml_read("minimal", dirname(path_yml))),
    "Expected an ordered map")
})


test_that("Filenames must be unique across artefacts", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  yaml_write(list(script = "script.R",
                  resources = "data.csv",
                  artefacts = list(list(
                    staticgraph = list(
                      description = "a graph",
                      filenames = c("mygraph.png", "mygraph.png"))))),
             path_yml)
  expect_error(
    orderly_yml_read("minimal", dirname(path_yml)),
    "Duplicate artefact filenames are not allowed: 'mygraph.png'")
})


test_that("README.md may not be listed as an artefact", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  yaml_write(list(script = "script.R",
                  resources = "data.csv",
                  artefacts = list(list(
                    staticgraph = list(
                      description = "a graph",
                      filenames = c("mygraph.png", "README.md"))))),
             path_yml)
  expect_error(
    orderly_yml_read("minimal", dirname(path_yml)),
    "README.md should not be listed as an artefact")
})


test_that("Filenames must be a character vector", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  writeLines(c(
    "script: script.R",
    "artefacts:",
    "  staticgraph:",
    "    description: A graph of things",
    "    filenames: 1"),
    path_yml)
  expect_error(
    orderly_yml_read("minimal", dirname(path_yml)),
    "orderly.yml:artefacts[1]:description must be character (check entry 1)",
    fixed = TRUE)
})


test_that("Filenames must be a character vector", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  writeLines(c(
    "script: script.R",
    "artefacts:",
    "  graph:",
    "    description: A graph of things",
    "    filenames: mygraph.png"),
    path_yml)
  expect_error(
    orderly_yml_read("minimal", dirname(path_yml)),
    "orderly.yml:artefacts[1]'; should be one of 'staticgraph'",
    fixed = TRUE)
})


test_that("packages must be a character vector", {
  expect_null(
    orderly_yml_validate_packages(NULL, "orderly.yml"))
  expect_equal(
    orderly_yml_validate_packages(c("a", "b", "c"), "orderly.yml"),
    c("a", "b", "c"))
  expect_error(
    orderly_yml_validate_packages(c(1, 2, 3), "orderly.yml"),
    "'orderly.yml:packages' must be character")
})


test_that("sources must exist", {
  expect_error(
    orderly_yml_validate_sources(c("a", "b", "c"), "orderly.yml"),
    "Source file does not exist: 'a', 'b', 'c'")

  withr::with_tempdir({
    file.create(c("a", "b", "c"))
    expect_equal(
      orderly_yml_validate_sources(c("a", "b", "c"), "orderly.yml"),
      c("a", "b", "c"))
  })
})


test_that("displayname must be a scalar character", {
  expect_equal(
    orderly_yml_validate_displayname("desc", "orderly.yml"),
    "desc")
  expect_error(
    orderly_yml_validate_displayname(letters, "orderly.yml"),
    "'orderly.yml:displayname' must be a scalar")
})


test_that("description must be a scalar character", {
  expect_equal(
    orderly_yml_validate_description("desc", "orderly.yml"),
    "desc")
  expect_error(
    orderly_yml_validate_description(letters, "orderly.yml"),
    "'orderly.yml:description' must be a scalar")
})


test_that("parameters can be handled", {
  expect_equal(
    orderly_yml_validate_parameters(list(a = NULL, b = NULL), "orderly.yml"),
    list(a = NULL, b = NULL))
  expect_equal(
    orderly_yml_validate_parameters(list(a = list(default = 1), b = NULL),
                                    "orderly.yml"),
    list(a = list(default = 1), b = NULL))

  ## Was deprecated since ages ago
  expect_error(
    orderly_yml_validate_parameters(c("a", "b"), "orderly.yml"),
    "'orderly.yml:parameters' must be named")

  expect_error(
    orderly_yml_validate_parameters(list(a = 1, b = 2), "orderly.yml"),
    "'orderly.yml:parameters:a' must be named")
  expect_error(
    orderly_yml_validate_parameters(list(a = list(type = "number"), b = NULL),
                                    "orderly.yml"),
    "Unknown fields in orderly.yml:parameters:a: type")
})


test_that("Dependencies can be declared", {
  path <- test_prepare_orderly_example(c("minimal", "depend"))
  dat <- orderly_yml_read("depend", file.path(path, "src", "depend"))
  expect_equal(
    dat$depends,
    data.frame(id = "latest", name = "minimal", filename = "mygraph.png",
               as = "graph.png"))
})


test_that("Cope with both unordered and ordered lists", {
  d1 <- list(list(a = list(id = "id", use = list(to1 = "from1"))),
             list(b = list(id = "id", use = list(to2 = "from2"))))
  d2 <- list(a = list(id = "id", use = list(to1 = "from1")),
             b = list(id = "id", use = list(to2 = "from2")))
  expect_equal(
    orderly_yml_validate_depends(d1, "orderly.yml"),
    orderly_yml_validate_depends(d2, "orderly.yml"))
})


test_that("Validate use", {
  expect_error(
    orderly_yml_validate_depends(
      list(a = list(id = "id", use = list(to = c("x", "y")))), "orderly.yml"),
    "orderly.yml:depends:a:use must all be single strings")
})
