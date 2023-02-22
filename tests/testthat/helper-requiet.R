requiet <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressWarnings(suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  ))
}

# load hard dependencies
requiet("ggeffects")
requiet("insight")
