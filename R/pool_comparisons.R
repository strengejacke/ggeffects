#' Pool contrasts and comparisons from `test_predictions()`
#'
#' This function "pools" (i.e. combines) multiple `ggcomparisons` objects, returned
#' by [`test_predictions()`], in a similar fashion as [`mice::pool()`].
#'
#' @param x A list of `ggcomparisons` objects, as returned by [`test_predictions()`].
#' @param ... Currently not used.
#'
#' @details Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
#'
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York:
#' John Wiley and Sons.
#'
#' @examplesIf require("mice")
#' data("nhanes2", package = "mice")
#' imp <- mice::mice(nhanes2, printFlag = FALSE)
#' comparisons <- lapply(1:5, function(i) {
#'   m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
#'   test_predictions(m, "age")
#' })
#' pool_comparisons(comparisons)
#' @return A data frame with pooled comparisons or contrasts of predictions.
#' @export
pool_comparisons <- function(x, ...) {

  # check input -----

  obj_name <- deparse(substitute(x), width.cutoff = 500)
  original_x <- x

  if (!all(vapply(x, inherits, logical(1), "ggcomparisons"))) {
    insight::format_error(
      "`x` must be a list of `ggcomparisons` objects, as returned by `test_predictions()`."
    )
  }

  # we need to check if all objects are comparible. We check whether columns
  # and values of focal terms are identical across all objects.
  estimate_name <- attributes(x[[1]])$estimate_name
  estimate_cols <- which(colnames(x[[1]]) == estimate_name) - 1

  # check if all comparisons/contrasts are identical
  result <- x[[1]][estimate_cols]
  if (!all(vapply(2:length(x), function(i) identical(x[[i]][estimate_cols], result), logical(1)))) {
    insight::format_error(paste0(
      "Cannot pool results from prediction tests. The values of the focal term(s) ",
      toString(colnames(x[[1]])[estimate_cols]),
      " are not identical across predictions."
    ))
  }

  # preparation ----

  len <- length(x)
  ci <- attributes(x[[1]])$ci_level
  dof <- attributes(x[[1]])$df

  if (is.null(dof)) {
    dof <- Inf
  }

  # pool predictions -----

  pooled_predictions <- original_x[[1]]
  pooled_predictions$std.error <- NA
  n_rows <- nrow(original_x[[1]])

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(lapply(original_x, function(j) {
      j[[estimate_name]][i]
    }), use.names = FALSE)
    pooled_predictions[[estimate_name]][i] <- mean(pooled_pred, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) {
      attributes(j)$standard_error[i]
    }), use.names = FALSE)
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_pred)
    pooled_predictions$std.error[i] <- sqrt(tmp)
  }

  # confidence intervals ----

  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = dof)
  pooled_predictions$conf.low <- pooled_predictions[[estimate_name]] - fac * pooled_predictions$std.error
  pooled_predictions$conf.high <- pooled_predictions[[estimate_name]] + fac * pooled_predictions$std.error

  attributes(pooled_predictions) <- utils::modifyList(attributes(original_x[[1]]), attributes(pooled_predictions))
  attr(pooled_predictions, "standard_error") <- pooled_predictions$std.error
  pooled_predictions$std.error <- NULL

  pooled_predictions
}
