test_that("Can run simple example with plugin", {
  path <- test_prepare_orderly_example("minimal")

  writeLines(c(
    "plugins:",
    "  - orderly2.random",
    "orderly2.random:",
    "  distribution:",
    "    normal"),
    file.path(path, "orderly_config.yml"))
  writeLines(c(
    "script: script.R",
    "artefacts:",
    "  data:",
    "    description: Generated data",
    "    filenames: data.rds",
    "orderly2.random:",
    "  dat: 10"),
    file.path(path, "src/minimal/orderly.yml"))
  writeLines(
    "saveRDS(dat, 'data.rds')",
    file.path(path, "src/minimal/script.R"))

  unlink(file.path(path, "src/minimal/data.csv"))

  register_example_plugin()
  env <- new.env()
  set.seed(1)
  id <- orderly_run("minimal", root = path, envir = env)

  set.seed(1)
  cmp <- rnorm(10)

  root <- orderly_root(path, locate = FALSE)
  meta <- root$outpack$metadata(id, full = TRUE)

  ## Our nice vectors have become lists here, due to the general pain
  ## of deserialising json, into R but at least it's all there.
  ## Probably the most general solution involves plugins being able to
  ## provide deserialisers that can apply any required simplification?
  expect_equal(meta$custom$orderly$plugins$orderly2.random,
               list(dat = list(mean(cmp), var(cmp))))
  expect_equal(readRDS(file.path(path, "archive", "minimal", id, "data.rds")),
               cmp)
})
