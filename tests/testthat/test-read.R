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
