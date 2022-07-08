test_that("Can run simple example", {
  path <- test_prepare_orderly_example("minimal")

  env <- new.env()
  id <- orderly_run("minimal", root = path, envir = env)
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


test_that("Can run an example with parameters", {
  path <- test_prepare_orderly_example("parameters")
  expect_error(orderly_run("parameters", root = path),
               "Missing parameters: 'a', 'b'")
  env <- new.env()
  id <- orderly_run("parameters", list(a = 1, b = 2), envir = env, root = path)

  dat <- readRDS(file.path(path, "archive", "parameters", id, "data.rds"))
  expect_equal(dat, list(a = 1, b = 2, c = 3))

  root <- orderly_root(path, FALSE)
  expect_equal(names(root$outpack$index()$metadata), id)

  expect_equal(root$outpack$metadata(id)$parameters, dat)
  expect_mapequal(as.list(env), dat)
})
