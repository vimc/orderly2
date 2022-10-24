test_that("Configuration must be empty", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  writeLines("a: 1", file.path(tmp, "orderly_config.yml"))
  expect_error(orderly_config(tmp),
               "Unknown fields in .+: a")
})


test_that("Configuration must exist", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  fs::dir_create(tmp)
  outpack::outpack_init(tmp)
  expect_error(orderly_config(tmp),
               "Orderly configuration does not exist: 'orderly_config.yml'")
  expect_error(orderly_root(tmp, FALSE),
               "Orderly configuration does not exist: 'orderly_config.yml'")
})


test_that("Initialisation requires empty directory", {
  tmp <- tempfile()
  fs::dir_create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  file.create(file.path(tmp, "file"))
  expect_error(orderly_init(tmp),
               "'path', if it already exists, must be an empty directory")
})


test_that("Can initialise a new orderly root", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  root <- orderly_init(tmp)
  expect_true(file.exists(tmp))
  expect_s3_class(root, "orderly_root")
  expect_s3_class(root$outpack, "outpack_root")
  expect_equal(root$config, list(plugins = NULL))
})
