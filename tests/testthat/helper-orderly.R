options(outpack.schema_validate = TRUE)


test_prepare_orderly_example <- function(examples, ...) {
  tmp <- tempfile()
  withr::defer_parent(unlink(tmp, recursive = TRUE))
  orderly_init(tmp)
  fs::dir_create(file.path(tmp, "src"))
  for (i in examples) {
    fs::dir_copy(file.path("examples", i), file.path(tmp, "src"))
  }
  tmp
}


append_lines <- function(new, path) {
  txt <- readLines(path)
  writeLines(c(txt, new), path)
}


json_to_df <- function(x) {
  nms <- lapply(x, names)
  stopifnot(length(unique(nms)) == 1)
  nms <- nms[[1]]
  dat <- lapply(nms, function(nm) sapply(x, "[[", nm))
  names(dat) <- nms
  as.data.frame(dat)
}
