#' Update latest ggeffects-version from R-universe (GitHub) or CRAN
#'
#' This function can be used to install the latest package version of *ggeffects*,
#' either the development version (from R-universe/GitHub) or the current
#' version from CRAN.
#'
#' @param source Character. Either `"development"` or `"cran"`. If `"cran"`,
#'   *ggeffects* will be installed from the default CRAN mirror returned by
#'   `getOption("repos")['CRAN']`. If `"development"` (the default), *ggeffects*
#'   is installed from the r-universe repository (<https://strengejacke.r-universe.dev/>).
#' @param force Logical, if `FALSE`, the update will only be installed if a newer
#'   version is available. Use `force=TRUE` to force installation, even if the
#'   version number for the locally installed package is identical to the latest
#'   development-version. Only applies when `source="development"`.
#' @param verbose Toggle messages.
#'
#' @return Invisible `NULL`.
#'
#' @examplesIf FALSE
#' # install latest development-version of ggeffects from the
#' # r-universe repository
#' install_latest()
#' @export
install_latest <- function(source = c("development", "cran"),
                           force = FALSE,
                           verbose = TRUE) {
  source <- match.arg(source, c("development", "cran"))
  needs_update <- TRUE

  if (source == "development") {
    repos <- "https://strengejacke.r-universe.dev"
  } else {
    repos <- getOption("repos")["CRAN"]
  }

  # only install newer versions?
  if (isFALSE(force) && source == "development") {
    insight::check_if_installed("jsonlite", reason = "to check for updates among development packages")
    if (isTRUE(verbose)) {
      insight::print_color("Looking for newer package version...\n", "blue")
    }
    # get current CRAN and local versions
    local_version <- utils::packageVersion("ggeffects")

    # for development versions, overwrite CRAN version with r-universe version
    js <- jsonlite::fromJSON("https://strengejacke.r-universe.dev/packages/ggeffects")
    dev_version <- js$Version[1]
    needs_update <- dev_version > local_version
  }

  if (!needs_update) {
    if (isTRUE(verbose)) {
      insight::print_color("`ggeffects` is up to date!\n", "green")
    }
    return(invisible())
  }

  utils::install.packages("ggeffects", repos = repos)
}
