get_predictions_rqs <- function(model, ...) {
  ## TODO: add support for multiple taus - predict() currently returns no CI
  insight::format_error("`ggpredict()` does not support `rqs` models. Use `ggemmeans()` instead.")
}
