#' @exportS3Method ggeffects::get_predictions
get_predictions.rma <- function(model,
                                data_grid,
                                terms = NULL,
                                ci_level = 0.95,
                                model_info = NULL,
                                type = NULL,
                                typical = NULL,
                                vcov = NULL,
                                vcov_args = NULL,
                                condition = NULL,
                                interval = NULL,
                                link_inverse = NULL,
                                bias_correction = FALSE,
                                verbose = TRUE,
                                ...) {
  mods_formula <- eval(model$call$mods)
  X_new <- model.matrix(mods_formula, data = data_grid)

  if ("(Intercept)" %in% colnames(X_new)) {
    newmods <- X_new[, colnames(X_new) != "(Intercept)", drop = FALSE]
  } else {
    newmods <- X_new
  }

  if (!is.null(newmods) && ncol(newmods) == 0) {
    newmods <- NULL
  }

  pr <- predict(
    model,
    newmods = newmods,
    level = ci_level * 100,
    ...
  )

  data_grid$predicted <- as.numeric(pr$pred)
  data_grid$conf.low  <- as.numeric(pr$ci.lb)
  data_grid$conf.high <- as.numeric(pr$ci.ub)

  data_grid
}
