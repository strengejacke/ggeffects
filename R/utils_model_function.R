get_predict_function <- function(model) {
  if (inherits(model, "logitr")) {
    "logitr"
  } else if (inherits(model, "glimML")) {
    "glimML"
  } else if (inherits(model, "gamlss")) {
    "gamlss"
  } else if (inherits(model, "sdmTMB")) {
    "sdmTMB"
  } else {
    "generic"
  }
}
