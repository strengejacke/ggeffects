#' @title Create a data frame from all combinations of predictor values
#' @name new_data
#'
#' @description Create a data frame for the "newdata"-argument that contains
#'   all combinations of values from the terms in questions. Similar to
#'   `expand.grid()`. The `terms`-argument accepts all shortcuts
#'   for representative values as in `predict_response()`.
#'
#' @param model A fitted model object.
#' @param terms Character vector with the names of those terms from `model` for
#'   which all combinations of values should be created. This argument works in
#'   the same way as the `terms` argument in `predict_response()`. See also
#'   [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html).
#' @param ... Currently not used.
#' @inheritParams predict_response
#' @inheritParams ggpredict
#'
#' @return A data frame containing one row for each combination of values of the
#'   supplied variables.
#'
#' @examplesIf requireNamespace("datawizard", quietly = TRUE)
#' data(efc, package = "ggeffects")
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' new_data(fit, c("c12hour [meansd]", "c161sex"))
#'
#' nd <- new_data(fit, c("c12hour [meansd]", "c161sex"))
#' pr <- predict(fit, type = "response", newdata = nd)
#' nd$predicted <- pr
#' nd
#'
#' # compare to
#' predict_response(fit, c("c12hour [meansd]", "c161sex"))
#'
#' @export
new_data <- function(model, terms, typical = "mean", condition = NULL, ...) {
  # get data
  mf <- .get_model_data(model)

  # check if we have a grouping variable in random effects, which we need
  # to convert to factors. This is a hidden option captured by "...", named
  # "group_factor".
  dots <- list(...)
  if (!is.null(dots$group_factor)) {
    for (g in dots$group_factor) {
      if (g %in% colnames(mf)) {
        mf[[g]] <- factor(mf[[g]])
      }
    }
  }

  .data_grid(
    model = model,
    model_frame = mf,
    terms = terms,
    value_adjustment = typical,
    factor_adjustment = TRUE,
    show_pretty_message = TRUE,
    condition = condition,
    emmeans.only = FALSE,
    verbose = FALSE
  )
}

#' @rdname new_data
#' @export
data_grid <- new_data
