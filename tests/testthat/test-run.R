test_that("Can run simple example", {
  path <- test_prepare_orderly_example("minimal")

  id <- orderly_run("minimal", root = path)
  expect_match(id, "^([0-9]{8}-[0-9]{6}-[[:xdigit:]]{8})$")
  p <- file.path(path, "archive", "minimal", id)
  expect_true(file.exists(p))

  root <- orderly_root(path, FALSE)
  idx <- root$outpack$index()
  expect_equal(names(idx$metadata), id)
  meta <- root$outpack$metadata(id, full = TRUE)

  ## Looks like we need some nice deserialisation support here if
  ## we're going to be able to pull this in nicely. That suggests that
  ## we should register a deserialiser requiring a little work in
  ## outpack and also probably saving the package version into the
  ## custom data. Then when outpack sees custom data on load it will
  ## pass it through.
  expect_equal(
    meta$custom$orderly$artefacts,
    list(list(description = "A graph of things",
              paths = list("mygraph.png"))))
})
