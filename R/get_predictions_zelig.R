get_predictions_zelig <- function(model, fitfram, ci_level, linv, ...) {

  insight::format_error("`predict_response()` does currently not support Zelig-models.")

  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level))
    ci <- (1 + ci_level) / 2
  else
    ci <- 0.975

  # prediction, with CI
  # prdat <-
  #   Zelig::predict(
  #     model,
  #     newdata = fitfram,
  #     interval = se,
  #     level = ci,
  #     ...
  #   )

  NULL
}
