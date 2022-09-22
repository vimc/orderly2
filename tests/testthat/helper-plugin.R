example_plugin <- function() {
  list(
    check = function(data, filename) {
      assert_named(data, name = paste0(filename, ":orderly.random"))
      assert_scalar_character(data$distribution,
                              paste0(filename, ":orderly.random:distribution"))
      data$generator <- switch(
        data$distribution,
        normal = rnorm,
        uniform = runif,
        stop(sprintf("Unknown value '%s' for '%s:orderly.random:distribution'",
                     data$distribution, filename)))
      data
    },

    read = function(data, filename, root) {
      assert_named(data, name = paste0(filename, ":orderly.random"))
      for (i in names(data)) {
        assert_scalar_numeric(data[[i]],
                              sprintf("%s:orderly.random:%s", filename, i))
      }
      data
    },

    run = function(data, config, envir, parameters, path) {
      generator <- config$generator
      meta <- list()
      for (i in names(data)) {
        r <- generator(data[[i]])
        envir[[i]] <- r
        meta[[i]] <- c(mean(r), var(r))
      }
      meta
    })
}


register_example_plugin <- function() {
  dat <- example_plugin()
  orderly_register_plugin("orderly2.random",
                          orderly_plugin(dat$check, dat$read, dat$run))
  withr::defer_parent(rm(list = "orderly2.random", envir = .plugins))
}


clear_plugins <- function() {
  rm(list = ls(envir = .plugins), envir = .plugins)
}
