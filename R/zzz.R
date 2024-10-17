.onLoad <- function(libname, pkgname) {
  # session warnings
  options("ggeffects_warning_bias_correction" = TRUE)
}

.onUnload <- function(libpath) {
  options("ggeffects_warning_bias_correction" = NULL)
}
