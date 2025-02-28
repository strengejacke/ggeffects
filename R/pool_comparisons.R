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

  if (!all(vapply(x, inherits, logical(1), c("ggcomparisons", "estimate_contrasts")))) {
    insight::format_error(
      "`x` must be a list of `ggcomparisons` objects, as returned by `test_predictions()`."
    )
  }

  # we need to check if all objects are comparible. We check whether columns
  # and values of focal terms are identical across all objects.
  estimate_name <- attributes(x[[1]])$coef_name

    # preparation ----

  len <- length(x)
  ci <- attributes(x[[1]])$ci
  dof <- insight::get_df(attributes(x[[1]])$model, type = "wald", verbose = FALSE)

  if (is.null(dof)) {
    dof <- Inf
  }

  # pool predictions -----

  pooled_comparisons <- original_x[[1]]
  pooled_comparisons$standard.error <- NA
  n_rows <- nrow(original_x[[1]])

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_comp <- unlist(lapply(original_x, function(j) {
      j[[estimate_name]][i]
    }), use.names = FALSE)
    pooled_comparisons[[estimate_name]][i] <- mean(pooled_comp, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) j$standard.error[i]), use.names = FALSE)
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_comp)
    pooled_comparisons$standard.error[i] <- sqrt(tmp)
  }

  # pooled degrees of freedom for t-statistics
  pooled_df <- .barnad_rubin(
    m = nrow(pooled_comparisons),
    b = stats::var(pooled_comparisons[[estimate_name]]),
    t = pooled_comparisons$standard.error^2,
    dfcom = dof
  )

  # confidence intervals ----
  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = dof)
  pooled_comparisons$conf.low <- pooled_comparisons[[estimate_name]] - fac * pooled_comparisons$standard.error
  pooled_comparisons$conf.high <- pooled_comparisons[[estimate_name]] + fac * pooled_comparisons$standard.error

  attributes(pooled_comparisons) <- utils::modifyList(attributes(original_x[[1]]), attributes(pooled_comparisons))
  attr(pooled_comparisons, "standard_error") <- pooled_comparisons$standard.error
  pooled_comparisons$standard.error <- NULL

  pooled_comparisons
}
