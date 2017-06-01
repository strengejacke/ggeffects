#' @importFrom stats model.frame
get_model_frame <- function(model, fe.only = TRUE) {
  if (inherits(model, c("merMod", "lmerMod", "glmerMod", "nlmerMod", "merModLmerTest")))
    fitfram <- stats::model.frame(model, fixed.only = fe.only)
  else if (inherits(model, "lme"))
    fitfram <- model$data
  else
    fitfram <- stats::model.frame(model)

  fitfram
}
