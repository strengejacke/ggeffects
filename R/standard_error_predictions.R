# get standard errors of predictions from model matrix and vcov ----

.standard_error_predictions <- function(
  model,
  prediction_data,
  value_adjustment,
  terms,
  model_class = NULL,
  type = "fe",
  vcov.fun = NULL,
  vcov.type = NULL,
  vcov.args = NULL,
  condition = NULL,
  interval = NULL) {

  se <- tryCatch(
    {
      .safe_se_from_vcov(
        model,
        prediction_data,
        value_adjustment,
        terms,
        model_class,
        type,
        vcov.fun,
        vcov.type,
        vcov.args,
        condition,
        interval
      )
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  if (is.null(se) || inherits(se, c("error", "simpleError"))) {
    insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
    if (inherits(se, c("error", "simpleError"))) {
      cat(sprintf("* Reason: %s\n", .safe_deparse(se[[1]])))
      err.source <- .safe_deparse(se[[2]])
      if (all(grepl("^(?!(safe_se_from_vcov))", err.source, perl = TRUE))) {
        cat(sprintf("* Source: %s\n", err.source))
      }
    }
    se <- NULL
  }

  se
}

#' @importFrom stats model.matrix terms formula
#' @importFrom insight find_random clean_names find_parameters get_varcov find_formula
.safe_se_from_vcov <- function(model,
                              prediction_data,
                              value_adjustment,
                              terms,
                              model_class,
                              type,
                              vcov.fun,
                              vcov.type,
                              vcov.args,
                              condition,
                              interval) {

  model_frame <- insight::get_data(model)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)


  # we can't condition on categorical variables

  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- sapply(cn, function(.x) is.factor(model_frame[[.x]]) && !(.x %in% re.terms))
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
    condition = condition
  )

  # make sure we have enough values to compute CI
  # nlevels_terms <- sapply(
  #   colnames(newdata),
  #   function(.x) !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1
  # )

  # if (any(nlevels_terms)) {
  #   not_enough <- colnames(newdata)[which(nlevels_terms)[1]]
  #   remove_lvl <- paste0("[", gsub(pattern = "(.*)\\[(.*)\\]", replacement = "\\2", x = terms[which(.clean_terms(terms) == not_enough)]), "]", collapse = "")
  #   stop(sprintf("`%s` does not have enough factor levels. Try to remove `%s`.", not_enough, remove_lvl), call. = TRUE)
  # }


  # add response to newdata. For models fitted with "glmmPQL",
  # the response variable is renamed internally to "zz".

  if (inherits(model, "glmmPQL")) {
    new.resp <- 0
    names(new.resp) <- "zz"
  } else {
    fr <- insight::find_response(model, combine = FALSE)
    new.resp <- rep(0, length.out = length(fr))
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

  vmatrix <- .vcov_helper(model, model_frame, model_class, newdata, vcov.fun, vcov.type, vcov.args, terms)
  pvar <- diag(vmatrix)
  pr_int <- FALSE

  # condition on random effect variances
  if (type == "re" || (!is.null(interval) && interval == "prediction")) {
    sig <- .get_residual_variance(model)
    if (!is.null(sig) && sig > 0.0001) {
      pvar <- pvar + sig
      pr_int <- TRUE
    }
  }

  se.fit <- sqrt(pvar)

  n_pred <- nrow(prediction_data)
  n_se <- length(se.fit)

  # shorten to length of prediction_data
  if (!is.null(model_class) && model_class %in% c("polr", "multinom", "mixor")) {
    se.fit <- rep(se.fit, each = .n_distinct(prediction_data$response.level))
  } else if (type == "re" && n_se < n_pred && n_pred %% n_se == 0) {
    se.fit <- rep(se.fit, times = n_pred / n_se)
  } else {
    se.fit <- se.fit[1:n_pred]
  }

  std_error <- list(prediction_data = prediction_data, se.fit = se.fit)
  attr(std_error, "prediction_interval") <- pr_int

  std_error
}
