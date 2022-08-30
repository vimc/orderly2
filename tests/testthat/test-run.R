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


test_that("Check that required files are produced", {
  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  dat <- yaml_read(path_yml)
  dat$artefacts[[1]]$filenames <- c(dat$artefacts[[1]]$filenames, "other.png")
  yaml_write(dat, path_yml)

  env <- new.env()
  expect_error(orderly_run("minimal", root = path, envir = env),
               "Script did not produce expected artefacts: 'other.png'")

  id <- dir(file.path(path, "draft", "minimal"))
  expect_length(id, 1)
  draft <- file.path(path, "draft", "minimal", id)

  ## TODO: we might add something nice to expose the status of a
  ## currently running packet.
  expect_error(outpack::outpack_packet_end(),
               "No current packet")

  ## TODO: Still need something much nicer in outpack for this:
  root <- orderly_root(path, FALSE)
  idx <- root$outpack$index()
  expect_null(idx$metadata)
})


test_that("Notify if additional files are created", {
  path <- test_prepare_orderly_example("minimal")
  path_script <- file.path(path, "src", "minimal", "script.R")

  env <- new.env()
  expect_silent(
    id1 <- orderly_run("minimal", root = path, envir = env))

  append_lines("file.create('other.txt')", path_script)
  expect_message(
    id2 <- orderly_run("minimal", root = path, envir = env),
    "Some extra files found: 'other.txt'")

  root <- orderly_root(path, FALSE)
  idx <- root$outpack$index()
  expect_equal(names(idx$metadata), c(id1, id2))

  meta <- root$outpack$metadata(id2)
  ## Different to orderly1 - later we'll ignore these (mrc-3382)
  expect_true("other.txt" %in% meta$files$path)
})


test_that("Can read functions in from source", {
  path <- test_prepare_orderly_example("source")
  env <- new.env()
  id <- orderly_run("source", root = path, envir = env)
  expect_equal(ls(env), "average")
  expect_equal(
    readRDS(file.path(path, "archive", "source", id, "data.rds")),
    apply(mtcars, 2, mean))

  root <- orderly_root(path, FALSE)
  meta <- root$outpack$metadata(id, full = TRUE)
  role <- json_to_df(meta$custom$orderly$role)
  expect_true("functions.R" %in% role$path)
  expect_equal(role$role[role$path == "functions.R"], "source")
})


test_that("Can load packages", {
  skip_if_not_installed("mockery")
  mock_library <- mockery::mock()

  path <- test_prepare_orderly_example("minimal")
  path_yml <- file.path(path, "src", "minimal", "orderly.yml")
  append_lines("packages:\n  - a\n  - b", path_yml)

  mockery::stub(orderly_run, "library", mock_library)

  env <- new.env()
  id <- orderly_run("minimal", root = path, envir = env)

  mockery::expect_called(mock_library, 2)
  expect_equal(mockery::mock_args(mock_library)[[1]],
               list("a", character.only = TRUE))
  expect_equal(mockery::mock_args(mock_library)[[2]],
               list("b", character.only = TRUE))
})


test_that("clean parameters when none are specified", {
  expect_null(check_parameters(NULL, list()))
  expect_null(check_parameters(list(), list()))
  expect_error(check_parameters(list(a = 1), list()),
               "Extra parameters: 'a'")
  expect_error(check_parameters(list(a = 1, b = 2), list()),
               "Extra parameters: 'a', 'b'")
})


test_that("clean parameters given spec", {
  info <- list(a = NULL, b = list(default = 2))
  expect_equal(check_parameters(list(a = 10, b = 20), info),
               list(a = 10, b = 20))
  expect_equal(check_parameters(list(a = 1), info), list(a = 1, b = 2))

  expect_error(check_parameters(NULL, info),
               "Missing parameters: 'a'")
  expect_error(check_parameters(list(), info),
               "Missing parameters: 'a'")
  expect_error(check_parameters(list(a = 1, c = 3, d = 4), info),
               "Extra parameters: 'c', 'd'")
})


test_that("can run packet with dependency", {
  path <- test_prepare_orderly_example(c("minimal", "depend"))
  env <- new.env()
  id1 <- orderly_run("minimal", root = path, envir = env)
  id2 <- orderly_run("depend", root = path, envir = env)

  root <- orderly_root(path, FALSE)
  meta <- root$outpack$metadata(id2, full = TRUE)
  expect_equal(meta$depends$packet, id1)
  expect_equal(meta$depends$files[[1]],
               data_frame(here = "graph.png", there = "mygraph.png"))
  expect_true(file.exists(
    file.path(path, "archive", "depend", id2, "graph.png")))
})


test_that("can fail with informative error if dependency not found", {
  path <- test_prepare_orderly_example(c("minimal", "depend"))
  env <- new.env()
  expect_error(
    orderly_run("depend", root = path, envir = env),
    "Failed to resolve dependency 'minimal:latest'")
})


test_that("can use global resources", {
  path <- test_prepare_orderly_example("global")
  id <- orderly_run("global", root = path)

  ## File copied and renamed
  path_dst <- file.path(path, "archive", "global", id, "global_data.csv")
  expect_true(file.exists(path_dst))
  expect_equal(hash_file(path_dst),
               hash_file(file.path(path, "global", "data.csv")))

  ## Added to metadata:
  root <- orderly_root(path, FALSE)
  meta <- root$outpack$metadata(id, full = TRUE)

  ## TODO: we should have some sort of custom metadata deserialisation
  ## help
  role <- data_frame(
    role = vapply(meta$custom$orderly$role, "[[", "", "role"),
    path = vapply(meta$custom$orderly$role, "[[", "", "path"))
  expect_equal(sum(role$role == "global"), 1)
  expect_equal(role$path[role$role == "global"], "global_data.csv")

  expect_equal(meta$custom$orderly$global,
               list(list(here = "global_data.csv",
                         there = "data.csv")))
})
