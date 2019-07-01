#' @importFrom insight find_terms
.back_transform_response <- function(model, mydf, back.transform) {
  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale

  ## TODO remove once insight 0.4.0 is on CRAN
  if (packageVersion("insight") >= "0.4.0")
    rv <- insight::find_terms(model)[["response"]]
  else
    rv <- insight::find_variables(model)[["response"]]

  if (any(grepl("log\\((.*)\\)", rv))) {
    if (back.transform) {
      # do we have log-log models?
      if (grepl("log\\(log\\((.*)\\)\\)", rv)) {
        mydf$predicted <- exp(exp(mydf$predicted))
        if (obj_has_name(mydf, "conf.low") && obj_has_name(mydf, "conf.high")) {
          mydf$conf.low <- exp(exp(mydf$conf.low))
          mydf$conf.high <- exp(exp(mydf$conf.high))
        }
      } else {
        mydf$predicted <- exp(mydf$predicted)
        if (obj_has_name(mydf, "conf.low") && obj_has_name(mydf, "conf.high")) {
          mydf$conf.low <- exp(mydf$conf.low)
          mydf$conf.high <- exp(mydf$conf.high)
        }
      }

      message("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.")
    } else {
      message("Model has log-transformed response. Predictions are on log-scale.")
    }
  }

  mydf
}



# name and sort columns, depending on groups, facet and panel
prepare_columns <- function(mydf, cleaned.terms) {
  columns <- c("x", "predicted", "conf.low", "conf.high", "response.level", "group", "facet", "panel", "observed", "residuals")

  # with or w/o grouping factor?
  if (length(cleaned.terms) == 1) {
    colnames(mydf)[1] <- "x"
    # convert to factor for proper legend
    mydf$group <- sjmisc::to_factor(1)
  } else if (length(cleaned.terms) == 2) {
    colnames(mydf)[1:2] <- c("x", "group")
  } else if (length(cleaned.terms) == 3) {
    colnames(mydf)[1:3] <- c("x", "group", "facet")
  } else if (length(cleaned.terms) == 4) {
    colnames(mydf)[1:4] <- c("x", "group", "facet", "panel")
  }

  # sort columns
  mydf[, columns[columns %in% colnames(mydf)]]
}
