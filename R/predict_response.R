#' @param marginalize Character string, indicating how to marginalize over the
#' *non-focal* predictors, i.e. those variables that are *not* specified in
#' `terms`. Possible values are `"mean_reference"`, `"mean_mode"`,
#' `"marginalmeans"`, `"empirical"` (or its alias, `"counteractual"`), and
#' `"full_data"`. You can set a default-option for the `marginalize` argument via
#' `options()`, e.g. `options(ggeffects_marginalize = "empirical")`, so you don't
#' have to specify your preferred marginalization method each time you call
#' `predict_response()`.
#'
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
  # default for "marginalize" argument?
  marginalize <- getOption("ggeffects_marginalize", marginalize)
  # validate "marginalize" argument
  marginalize <- match.arg(
    marginalize,
    c("mean_reference", "mean_mode", "marginalmeans", "empirical",
      "counterfactual", "full_data")
  )

  # save name, so it can later be retrieved from environment
  model_name <- insight::safe_deparse(substitute(model))

  # validate type arguments
  type_and_ppd <- .validate_type_argument(type, ppd)
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
      ci_level = ci_level,
      type = type,
      typical = "mean",
      condition = condition,
      back_transform = back_transform,
      ppd = ppd,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      verbose = verbose,
      ...
    ),
    mean_mode = ggpredict(
      model,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = c(numeric = "mean", factor = "mode"),
      condition = condition,
      back_transform = back_transform,
      ppd = ppd,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      interval = interval,
      verbose = verbose,
      ...
    ),
    marginalmeans = ggemmeans(
      model,
      terms = terms,
      ci_level = ci_level,
      type = type,
      typical = "mean",
      condition = condition,
      back_transform = back_transform,
      interval = interval,
      verbose = verbose,
      ...
    ),
    counterfactual = ,
    empirical = ggaverage(
      model,
      terms = terms,
      ci_level = ci_level,
      typical = "mean",
      condition = condition,
      back_transform = back_transform,
      vcov_fun = vcov_fun,
      vcov_type = vcov_type,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    ),
    full_data = {
      ## TODO: implement
      # should be:
      # marginaleffects::predictions(
      #   model,
      #   newdata = insight::get_data(model),
      #   by = terms
      # )
    }
  )

  attr(out, "model.name") <- model_name
  out
}
