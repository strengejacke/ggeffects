#' @param marginalize Character string. How to marginalize over the predictors.
#' Possible values are `"mean_reference"`, `"mean_mode"`, `"marginalmean"`, and
#' `"empirical"`. To do...
#' @rdname ggpredict
#' @export
predict_response <- function(model,
                             terms,
                             marginalize = "mean_reference",
                             ci_level = 0.95,
                             type = "fixed",
                             condition = NULL,
                             back_transform = TRUE,
                             ppd = FALSE,
                             vcov_fun = NULL,
                             vcov_type = NULL,
                             vcov_args = NULL,
                             interval,
                             verbose = TRUE,
                             ...) {
  # validate "marginalize argument"
  marginalize <- match.arg(marginalize, c("mean_reference", "mean_mode", "marginalmean", "empirical"))

  # validate type arguments
  type_and_ppd <- .validate_type_argument(type)
  type <- type_and_ppd$type
  ppd <- type_and_ppd$ppd

  if (missing(interval)) {
    if (type %in% c("re", "re.zi")) {
      interval <- "prediction"
    } else {
      interval <- "confidence"
    }
  }

  out <- switch(marginalize,
    mean_reference = ggpredict(
      model,
      terms = terms,
      condition = condition,
      type = type,
      back_transform = back_transform,
      ppd = ppd,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      verbose,
      ...
    ),
    mean_mode = ggpredict(
      model,
      terms = terms,
      condition = condition,
      type = type,
      typical = c(numeric = "mean", factor = "mode"),
      back_transform = back_transform,
      ppd = ppd,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      verbose,
      ...
    ),
    marginalmean = ggemmeans(
      model,
      terms = terms,
      condition = condition,
      type = type,
      back_transform = back_transform,
      interval = interval,
      verbose,
      ...
    ),
    empirical = {
      ## TODO: implement
      # should be:
      # marginaleffects::predictions(
      #   model,
      #   newdata = insight::get_data(model),
      #   by = terms
      # )
    }
  )
}
