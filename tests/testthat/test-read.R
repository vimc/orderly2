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
