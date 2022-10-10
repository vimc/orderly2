example_plugin <- function() {
  list(
    check = function(data, filename) {
      assert_named(data, name = paste0(filename, ":example.random"))
      assert_scalar_character(data$distribution,
                              paste0(filename, ":example.random:distribution"))
      data$generator <- switch(
        data$distribution,
        normal = rnorm,
        uniform = runif,
        stop(sprintf("Unknown value '%s' for '%s:example.random:distribution'",
                     data$distribution, filename)))
      data
    },

    read = function(data, filename, root) {
      assert_named(data, name = paste0(filename, ":example.random"))
      for (i in names(data)) {
        assert_scalar_numeric(data[[i]],
                              sprintf("%s:example.random:%s", filename, i))
      }
      data
    },

    run = function(data, root, parameters, envir, path) {
      generator <- root$config[["example.random"]]$generator
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
  orderly_plugin_register(orderly_plugin(dat$check, dat$read, dat$run),
                          "example.random")
  withr::defer_parent(rm(list = "example.random", envir = .plugins))
}


clear_plugins <- function() {
  rm(list = ls(envir = .plugins), envir = .plugins)
}
