#' Pool Predictions or Estimated Marginal Means
#'
#' This function "pools" (i.e. combines) multiple `ggeffects` objects, in
#' a similar fashion as [`mice::pool()`].
#'
#' @param x A list of `ggeffects` objects, as returned by [`predict_response()`].
#' @param ... Currently not used.
#'
#' @details Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
#' Pooling is applied to the predicted values on the scale of the *linear predictor*,
#' not on the response scale, in order to have accurate pooled estimates and
#' standard errors. The final pooled predicted values are then transformed to
#' the response scale, using [`insight::link_inverse()`].
#'
#' @references
#' Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York:
#' John Wiley and Sons.
#'
#' @examplesIf require("mice")
#' # example for multiple imputed datasets
#' data("nhanes2", package = "mice")
#' imp <- mice::mice(nhanes2, printFlag = FALSE)
#' predictions <- lapply(1:5, function(i) {
#'   m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
#'   predict_response(m, "age")
#' })
#' pool_predictions(predictions)
#' @return A data frame with pooled predictions.
#' @export
pool_predictions <- function(x, ...) {

  # check input -----

  obj_name <- deparse(substitute(x), width.cutoff = 500)
  original_x <- x

  if (!all(vapply(x, inherits, logical(1), "ggeffects"))) {
    insight::format_error(
      "`x` must be a list of `ggeffects` objects, as returned by `predict_response()`."
    )
  }

  # check if all x-levels are identical
  if (!all(apply(as.data.frame(sapply(x, function(i) i$x), simplify = TRUE), 1, function(j) length(unique(j)) == 1))) {
    insight::format_error(paste0(
      "Cannot pool predictions. The values of the focal term '",
      attributes(x[[1]])$terms,
      "' are not identical across predictions."
    ))
  }

  # preparation ----

  len <- length(x)
  ci <- attributes(x[[1]])$ci_level
  dof <- attributes(x[[1]])$df
  link_inv <- attributes(x[[1]])$link_inverse
  link_fun <- attributes(x[[1]])$link_function
  back_transform <- isTRUE(attributes(x[[1]])$back_transform)

  if (is.null(link_inv)) {
    link_inv <- function(x) x
  }
  if (is.null(link_fun)) {
    link_fun <- function(x) x
  }
  if (is.null(dof)) {
    dof <- Inf
  }

  # pool predictions -----

  pooled_predictions <- original_x[[1]]
  n_rows <- nrow(original_x[[1]])

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(lapply(original_x, function(j) {
      if (back_transform) {
        untransformed_predictions <- attributes(j)$untransformed_predictions
        if (!is.null(untransformed_predictions)) {
          link_fun(untransformed_predictions[i])
        } else {
          link_fun(j$predicted[i])
        }
      } else {
        link_fun(j$predicted[i])
      }
    }), use.names = FALSE)
    pooled_predictions$predicted[i] <- mean(pooled_pred, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(lapply(original_x, function(j) {
      j$std.error[i]
    }), use.names = FALSE)
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    tmp <- ubar + (1 + 1 / len) * stats::var(pooled_pred)
    pooled_predictions$std.error[i] <- sqrt(tmp)
  }

  # pooled degrees of freedom for t-statistics
  pooled_df <- .barnad_rubin(
    m = nrow(pooled_predictions),
    b = stats::var(pooled_predictions$predicted),
    t = pooled_predictions$std.error^2,
    dfcom = dof
  )

  # confidence intervals ----
  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = pooled_df)
  pooled_predictions$conf.low <- link_inv(pooled_predictions$predicted - fac * pooled_predictions$std.error)
  pooled_predictions$conf.high <- link_inv(pooled_predictions$predicted + fac * pooled_predictions$std.error)

  # backtransform
  pooled_predictions$predicted <- link_inv(pooled_predictions$predicted)

  # backtransform response
  if (back_transform) {
    pooled_predictions <- .back_transform_response(
      model = NULL,
      pooled_predictions,
      back_transform = TRUE,
      response.name = attributes(original_x[[1]])$response_transform
    )
  }

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


# helper ------

# adjustment for degrees of freedom
.barnad_rubin <- function(m, b, t, dfcom = 999999) {
  # fix for z-statistic
  if (is.null(dfcom) || all(is.na(dfcom)) || all(is.infinite(dfcom))) {
    return(Inf)
  }
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  result <- dfold * dfobs / (dfold + dfobs)
  pmax(round(mean(result, na.rm = TRUE)), 1)
}
