#' Pool Predictions or Estimated Marginal Means
#'
#' This function "pools" (i.e. combines) multiple \code{ggeffects} objects, in
#' a similar fashion as \code{mice::pool()}.
#'
#' @param x A list of \code{ggeffects} objects, as returned by
#'   \code{\link{ggpredict}}, \code{\link{ggemmeans}} or \code{\link{ggeffect}}.
#' @param ... Currently not used.
#'
#' @details Averaging of parameters follows Rubin's rules (\cite{Rubin, 1987, p. 76}).
#'
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York: John Wiley and Sons.
#'
#' @examples
#' # example for multiple imputed datasets
#' if (require("mice")) {
#'   data("nhanes2")
#'   imp <- mice(nhanes2, printFlag = FALSE)
#'   predictions <- lapply(1:5, function(i) {
#'     m <- lm(bmi ~ age + hyp + chl, data = complete(imp, action = i))
#'     ggpredict(m, "age")
#'   })
#'   pool_predictions(predictions)
#' }
#' @return A data frame with pooled predictions.
#' @export
pool_predictions <- function(x, ...) {

  # check input -----

  obj_name <- deparse(substitute(x), width.cutoff = 500)
  original_x <- x

  if (!all(sapply(x, inherits, "ggeffects"))) {
    stop("'x' must be a list of 'ggeffects' objects, as returned by 'ggpredict()', 'ggemmeans()' or 'ggeffect()'.", call. = FALSE)
  }

  # check if all x-levels are identical
  if (!all(apply(as.data.frame(sapply(x, function(i) i$x), simplify = TRUE), 1, function(j) length(unique(j)) == 1))) {
    stop(paste0("Cannot pool predictions. The values of the focal term '", attributes(x[[1]])$terms,"' are not identical across predictions."), call. = FALSE)
  }

  # preparation ----

  len <- length(x)
  ci <- attributes(x[[1]])$ci.lvl
  link_inv <- attributes(x[[1]])$link_inverse
  link_fun <- attributes(x[[1]])$link_function

  if (is.null(link_inv)) {
    link_inv <- function(x) x
  }
  if (is.null(link_fun)) {
    link_fun <- function(x) x
  }

  # pool predictions -----

  pooled_predictions <- original_x[[1]]
  n_rows <- nrow(original_x[[1]])

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(lapply(original_x, function(j) {
      link_fun(j$predicted[i])
    }))
    pooled_predictions$predicted[i] <- mean(pooled_pred, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) {
      j$std.error[i]
    }))
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_pred)
    pooled_predictions$std.error[i] <- sqrt(tmp)
  }

  # confidence intervals ----

  alpha <- (1 + ci) / 2
  fac <- stats::qnorm(alpha)
  pooled_predictions$conf.low <- link_inv(pooled_predictions$predicted - fac * pooled_predictions$std.error)
  pooled_predictions$conf.high <- link_inv(pooled_predictions$predicted + fac * pooled_predictions$std.error)

  # backtransform
  pooled_predictions$predicted <- link_inv(pooled_predictions$predicted)

  # constant values
  constant.values <- as.data.frame(do.call(rbind, lapply(original_x, function(x) {
    as.data.frame(attributes(x)$constant.values)
  })))

  attr(pooled_predictions, "constant.values") <- lapply(constant.values, function(i) {
    if (is.numeric(i)) {
      mean(i)
    } else {
      unique(i)
    }
  })

  pooled_predictions
}
