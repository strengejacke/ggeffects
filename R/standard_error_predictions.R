# get standard errors of predictions from model matrix and vcov ----

.standard_error_predictions <- function(
  model,
  prediction_data,
  value_adjustment,
  terms,
  model_class = NULL,
  type = "fixed",
  vcov = NULL,
  vcov_args = NULL,
  condition = NULL,
  interval = NULL,
  verbose = TRUE) {

  se <- tryCatch(
    .safe_se_from_vcov(
      model,
      prediction_data,
      value_adjustment,
      terms,
      model_class,
      type,
      vcov,
      vcov_args,
      condition,
      interval,
      verbose = verbose
    ),
    error = function(x) x,
    warning = function(x) NULL,
    finally = function(x) NULL
  )

  if (verbose && (is.null(se) || inherits(se, c("error", "simpleError")))) {
    insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
    if (inherits(se, c("error", "simpleError"))) {
      cat(sprintf("* Reason: %s\n", insight::safe_deparse(se[[1]])))
      err.source <- insight::safe_deparse(se[[2]])
      if (all(grepl("^(?!(safe_se_from_vcov))", err.source, perl = TRUE))) {
        cat(sprintf("* Source: %s\n", err.source))
      }
    }
    se <- NULL
  }

  se
}


.safe_se_from_vcov <- function(model,
                               prediction_data,
                               value_adjustment,
                               terms,
                               model_class,
                               type,
                               vcov,
                               vcov_args,
                               condition,
                               interval,
                               verbose = TRUE) {

  model_frame <- .get_model_data(model)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)


  # we can't condition on categorical variables

  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- vapply(cn, function(.x) is.factor(model_frame[[.x]]) && !(.x %in% re.terms), logical(1))
    condition <- condition[!cn.factors]
    if (.is_empty(condition)) condition <- NULL
  }


  # copy data frame with predictions
  newdata <- .data_grid(
    model,
    model_frame,
    terms,
    value_adjustment = value_adjustment,
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = condition,
    verbose = FALSE
  )

  # add response to newdata. For models fitted with "glmmPQL",
  # the response variable is renamed internally to "zz".

  if (inherits(model, "glmmPQL")) {
    new.resp <- 0
    names(new.resp) <- "zz"
  } else {
    fr <- insight::find_response(model, combine = FALSE)
    new.resp <- rep_len(0, length(fr))
    names(new.resp) <- fr
  }

  new.resp <- new.resp[setdiff(names(new.resp), colnames(newdata))]
  newdata <- cbind(as.list(new.resp), newdata)

  # clean terms from brackets
  terms <- .clean_terms(terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(terms) > 2) {
    trms <- terms[3]
    newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
    prediction_data <- prediction_data[order(prediction_data[[trms]]), , drop = FALSE]
  }

  if (length(terms) > 1) {
    trms <- terms[2]
    newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
    prediction_data <- prediction_data[order(prediction_data[[trms]]), , drop = FALSE]
  }

  trms <- terms[1]
  newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
  prediction_data <- prediction_data[order(prediction_data[[trms]]), , drop = FALSE]

  # rownames were resorted as well, which causes troubles in model.matrix
  rownames(newdata) <- NULL
  rownames(prediction_data) <- NULL

  vmatrix <- .safe(.vcov_helper(
    model,
    model_frame,
    model_class,
    newdata,
    vcov,
    vcov_args,
    terms,
    full.vcov = FALSE,
    verbose = verbose
  ))

  pr_int <- FALSE

  if (is.null(vmatrix) && verbose) {
    insight::format_alert("Could not compute variance-covariance matrix of predictions. No confidence intervals are returned.") # nolint
    se.fit <- NULL
  } else {
    pvar <- vmatrix

    # condition on random effect variances
    if (identical(interval, "prediction")) {
      sig <- .get_residual_variance(model)
      if (!is.null(sig) && sig > 0.0001) {
        pvar <- pvar + sig
        pr_int <- TRUE
      }
    }

    se.fit <- sqrt(pvar)

    n_pred <- nrow(prediction_data)
    n_se <- length(se.fit)
    match_len <- isTRUE(n_pred %% n_se == 0)

    # shorten to length of prediction_data
    if (!is.null(model_class) && model_class %in% c("polr", "multinom", "mixor", "multinom_weightit")) {
      se.fit <- rep(se.fit, each = .n_distinct(prediction_data$response.level))
    } else if (type == "random" && n_se < n_pred && match_len) {
      se.fit <- rep(se.fit, each = n_pred / n_se)
    } else {
      se.fit <- se.fit[1:n_pred]
    }
  }

  std_error <- list(prediction_data = prediction_data, se.fit = se.fit)
  attr(std_error, "prediction_interval") <- pr_int

  std_error
}
