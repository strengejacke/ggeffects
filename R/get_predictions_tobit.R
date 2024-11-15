#' @export
get_predictions.tobit <- function(model,
                                  data_grid = NULL,
                                  terms = NULL,
                                  ci_level = 0.95,
                                  type = NULL,
                                  typical = NULL,
                                  vcov = NULL,
                                  vcov_args = NULL,
                                  condition = NULL,
                                  interval = "confidence",
                                  bias_correction = FALSE,
                                  link_inverse = insight::link_inverse(model),
                                  model_info = NULL,
                                  verbose = TRUE,
                                  ...) {
  # does user want standard errors?
  se <- !is.null(ci_level) && !is.na(ci_level)

  # compute ci, two-ways
  if (!is.null(ci_level) && !is.na(ci_level)) {
    ci <- (1 + ci_level) / 2
  } else {
    ci <- 0.975
  }

  # degrees of freedom
  dof <- .get_df(model)
  tcrit <- stats::qt(ci, df = dof)

  # special handling for survival regression with type = "quantile"
  if (is.null(type) || !type %in% c("quantile", "uquantile")) {
    type <- "lp"
  }
  if (type == "quantile") {
    link_inverse <- function(x) x
  }

  prdat <- stats::predict(
    model,
    newdata = data_grid,
    type = type,
    se.fit = se,
    ...
  )

  # we need to shape data into long-format when type = "quantile",
  # but only when it's a matrix. Determine number of columns now
  if (is.matrix(prdat) || (!is.null(prdat$fit) && is.matrix(prdat$fit))) {
    if (is.null(prdat$fit)) {
      n_columns <- ncol(prdat)
    } else {
      n_columns <- ncol(prdat$fit)
    }
  } else {
    n_columns <- 1
  }

  # reshape here if we have more than one column (i.e. a matrix)
  if (type %in% c("quantile", "uquantile") && n_columns > 1) {
    # for type = "quantile", we get a matrix of predictions, with multiple
    # columns. we now duplicate the data grid and add the different status
    # options as "response" column to the data grid
    out <- NULL
    for (i in seq_len(n_columns)) {
      data_grid$response.level <- i
      out <- rbind(out, data_grid)
    }
    # if SE are requested, we need to gather multiple columns
    if (se) {
      prdat <- .multiple_gather(
        as.data.frame(prdat),
        names_to = "status",
        values_to = c("predicted", "se"),
        columns = list(seq_len(n_columns), n_columns + seq_len(n_columns))
      )
      prdat <- list(fit = prdat$predicted, se.fit = prdat$se)
    } else {
      prdat <- .gather(as.data.frame(prdat), "status", "predicted")$predicted
    }
    # we now have "prdat" in the same structure as for other types, so we
    # can proceed as usual from here...
    data_grid <- out
  }

  # did user request standard errors? if yes, compute CI
  if (se) {
    # copy predictions
    data_grid$predicted <- link_inverse(prdat$fit)

    # calculate CI
    data_grid$conf.low <- link_inverse(prdat$fit - tcrit * prdat$se.fit)
    data_grid$conf.high <- link_inverse(prdat$fit + tcrit * prdat$se.fit)

    # copy standard errors
    attr(data_grid, "std.error") <- prdat$se.fit

  } else {
    # copy predictions
    data_grid$predicted <- link_inverse(as.vector(prdat))

    # no CI
    data_grid$conf.low <- NA
    data_grid$conf.high <- NA
  }

  data_grid
}

#' @export
get_predictions.survreg <- get_predictions.tobit
