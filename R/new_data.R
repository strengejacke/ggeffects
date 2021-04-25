#' @title Create a data frame from all combinations of predictor values
#' @name new_data
#'
#' @description Create a data frame for the "newdata"-argument that contains
#'   all combinations of values from the terms in questions. Similar to
#'   \code{expand.grid()}. The \code{terms}-argument accepts all shortcuts
#'   for representative values as in \code{ggpredict()}.
#'
#' @param model A fitted model object.
#' @param terms Character vector with the names of those terms from
#'   \code{model} for which all combinations of values should be created.
#'
#' @inheritParams ggpredict
#'
#' @return A data frame containing one row for each combination of values of the
#'   supplied variables.
#'
#' @examples
#' data(efc)
#' fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' new_data(fit, c("c12hour [meansd]", "c161sex"))
#'
#' nd <- new_data(fit, c("c12hour [meansd]", "c161sex"))
#' pr <- predict(fit, type = "response", newdata = nd)
#' nd$predicted <- pr
#' nd
#'
#' # compare to
#' ggpredict(fit, c("c12hour [meansd]", "c161sex"))
#'
#' @export
new_data <- function(model, terms, typical = "mean", condition = NULL) {
  .data_grid(
    model = model,
    model_frame = insight::get_data(model),
    terms = terms,
    value_adjustment = typical,
    factor_adjustment = TRUE,
    show_pretty_message = TRUE,
    condition = condition,
    emmeans.only = FALSE
  )
}

#' @rdname new_data
#' @export
data_grid <- new_data
