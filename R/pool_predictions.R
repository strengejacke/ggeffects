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
#' For binomial-logit predictions integrated over a normal random intercept,
#' pooling uses the marginal logit and its delta-method standard error,
#' holding each fitted intercept standard deviation fixed.
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
  if (
    !all(apply(
      as.data.frame(sapply(x, function(i) i$x), simplify = TRUE),
      1,
      function(j) length(unique(j)) == 1
    ))
  ) {
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

  marginal_links <- lapply(x, function(prediction) {
    attr(attr(prediction, "link_inverse", exact = TRUE), "marginal_logit", exact = TRUE)
  })
  marginal <- !vapply(marginal_links, is.null, logical(1))
  if (any(marginal)) {
    if (!all(marginal)) {
      insight::format_error("All predictions must use the same marginal bias correction when pooling.")
    }
    original_x <- Map(function(prediction, transform) {
      result <- transform(.match_marginal_predictors(prediction))
      prediction$predicted <- result$estimate
      if (!is.null(prediction$std.error)) {
        prediction$std.error <- prediction$std.error * result$slope
      }
      prediction
    }, x, marginal_links)
    link_inv <- stats::plogis
    link_fun <- identity
    back_transform <- FALSE
  }

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
  b_vec <- numeric(n_rows)

  for (i in 1:n_rows) {
    # pooled estimate
    pooled_pred <- unlist(
      lapply(original_x, function(j) {
        untransformed_predictions <- attributes(j)$untransformed_predictions
        if (back_transform && !is.null(untransformed_predictions)) {
          predictions_to_process <- untransformed_predictions[i]
        } else {
          predictions_to_process <- j$predicted[i]
        }
        link_fun(predictions_to_process)
      }),
      use.names = FALSE
    )
    pooled_predictions$predicted[i] <- mean(pooled_pred, na.rm = TRUE)

    # pooled standard error
    pooled_se <- unlist(
      lapply(original_x, function(j) {
        j$std.error[i]
      }),
      use.names = FALSE
    )
    ubar <- mean(pooled_se^2, na.rm = TRUE)
    b_vec[i] <- stats::var(pooled_pred)
    tmp <- ubar + (1 + 1 / len) * b_vec[i]
    pooled_predictions$std.error[i] <- sqrt(tmp)
  }

  # pooled degrees of freedom for t-statistics
  pooled_df <- .barnad_rubin(
    m = len,
    b = b_vec,
    t = pooled_predictions$std.error^2,
    dfcom = dof
  )

  # confidence intervals ----
  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = pooled_df)
  pooled_predictions$conf.low <- link_inv(
    pooled_predictions$predicted - fac * pooled_predictions$std.error
  )
  pooled_predictions$conf.high <- link_inv(
    pooled_predictions$predicted + fac * pooled_predictions$std.error
  )

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
  constant.values <- as.data.frame(do.call(
    rbind,
    lapply(original_x, function(x) {
      as.data.frame(attributes(x)$constant.values)
    })
  ))

  attr(pooled_predictions, "constant.values") <- lapply(
    constant.values,
    function(i) {
      if (is.numeric(i)) {
        mean(i)
      } else {
        unique(i)
      }
    }
  )

  if (all(marginal)) {
    attr(pooled_predictions, "marginal_link_data") <- NULL
    attr(pooled_predictions, "untransformed_predictions") <- NULL
    attr(pooled_predictions, "back_transform") <- FALSE
    attr(pooled_predictions, "link_inverse") <- stats::plogis
    attr(pooled_predictions, "link_function") <- stats::qlogis
    attr(pooled_predictions, "standard_error") <- pooled_predictions$std.error
  }

  pooled_predictions
}


# helper ------

# Match by grid values so row subsetting or reordering preserves the link-scale data.
.match_marginal_predictors <- function(prediction) {
  link_data <- attr(prediction, "marginal_link_data", exact = TRUE)
  prediction_frame <- as.data.frame(prediction)
  keys <- intersect(names(prediction_frame), names(link_data))
  if (!"predicted" %in% keys) {
    insight::format_error("Marginal predictions must retain their original probability values for pooling.")
  }
  rows <- prediction_frame[keys]
  rows$.row <- seq_len(nrow(rows))
  link_data <- unique(link_data[c(keys, ".eta")])
  link_data$.matched <- TRUE
  matched <- merge(rows, link_data, by = keys, all.x = TRUE, sort = FALSE)
  if (nrow(matched) != nrow(rows) || anyNA(matched$.matched)) {
    insight::format_error("Cannot match marginal predictions to their original linear predictors.")
  }
  matched$.eta[order(matched$.row)]
}


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
  pmax(result, 1)
}
