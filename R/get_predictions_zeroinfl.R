#' @importFrom stats qlogis predict qnorm
get_predictions_zeroinfl <- function(model, data_grid, ci.lvl, linv, type, model_class, typical, terms, vcov.fun, vcov.type, vcov.args, condition, ...) {
  # get prediction type.
  pt <- if (model_class == "zeroinfl" && type == "fe")
    "count"
  else if (model_class == "zeroinfl" && type == "fe.zi")
    "response"
  else if (model_class == "zerotrunc" && type == "fe")
    "count"
  else if (model_class == "zerotrunc" && type == "fe.zi")
    "response"
  else if (model_class == "hurdle" && type == "fe")
    "count"
  else if (model_class == "hurdle" && type == "fe.zi")
    "response"
  else
    "response"

  # compute ci, two-ways
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- (1 + ci.lvl) / 2
  else
    ci <- .975


  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  if ("nsim" %in% names(add.args))
    nsim <- eval(add.args[["nsim"]])
  else
    nsim <- 1000


  # get predictions
  prdat <-
    stats::predict(
      model,
      newdata = data_grid,
      type = pt,
      ...
    )

  # need back-transformation
  data_grid$predicted <- log(as.vector(prdat))


  if (type == "fe.zi") {

    model_frame <- insight::get_data(model)
    clean_terms <- .clean_terms(terms)

    newdata <- .data_grid(
      model,
      model_frame,
      terms,
      value_adjustment = typical,
      factor_adjustment = FALSE,
      show_pretty_message = FALSE,
      condition = condition
    )

    prdat.sim <- .zeroinfl_predictions(model, newdata, nsim, terms, typical, condition)

    if (is.null(prdat.sim) || inherits(prdat.sim, c("error", "simpleError"))) {

      insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
      cat("Possibly a polynomial term is held constant (and does not appear in the `terms`-argument). Or try reducing number of simulation, using argument `nsim` (e.g. `nsim = 100`).\n")

      data_grid$predicted <- as.vector(prdat)
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA

    } else {

      sims <- exp(prdat.sim$cond) * (1 - stats::plogis(prdat.sim$zi))
      data_grid <- .zeroinflated_prediction_data(data_grid, newdata, as.vector(prdat), sims, ci, clean_terms)

      if (.obj_has_name(data_grid, "std.error")) {
        # copy standard errors
        attr(data_grid, "std.error") <- data_grid$std.error
        data_grid <- .remove_column(data_grid, "std.error")
      }

    }

  } else {

    # get standard errors from variance-covariance matrix
    se.pred <-
      .standard_error_predictions(
        model = model,
        fitfram = data_grid,
        typical = typical,
        type = type,
        terms = terms,
        model_class = model_class,
        vcov.fun = vcov.fun,
        vcov.type = vcov.type,
        vcov.args = vcov.args,
        condition = condition
      )


    if (!is.null(se.pred)) {

      se.fit <- se.pred$se.fit
      data_grid <- se.pred$fitfram

      # CI
      data_grid$conf.low <- linv(data_grid$predicted - stats::qnorm(ci) * se.fit)
      data_grid$conf.high <- linv(data_grid$predicted + stats::qnorm(ci) * se.fit)

      # copy standard errors
      attr(data_grid, "std.error") <- se.fit

    } else {
      # CI
      data_grid$conf.low <- NA
      data_grid$conf.high <- NA
    }

    data_grid$predicted <- linv(data_grid$predicted)

  }

  data_grid
}
