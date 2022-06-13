test_that("Can run simple example", {
  root <- tempfile()
  on.exit(unlink(root, recursive = TRUE))
  outpack::outpack_init(root)
  fs::dir_create(file.path(root, "src"))
  fs::dir_copy("examples/minimal", file.path(root, "src", "minimal"))

  id <- orderly_run("minimal", root = root)
  p <- file.path(root, "archive", "minimal", id)
  expect_true(file.exists(p))

  root <- orderly_root(root, FALSE)
  idx <- root$outpack$index()
  expect_equal(names(idx$metadata), id)
})
