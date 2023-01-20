.back_transform_response <- function(model, mydf, back.transform) {
  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale

  rv <- insight::find_terms(model)[["response"]]

  if (any(grepl("log\\((.*)\\)", rv))) {
    if (back.transform) {
      # do we have log-log models?
      if (grepl("log\\(log\\((.*)\\)\\)", rv)) {
        mydf$predicted <- exp(exp(mydf$predicted))
        if (.obj_has_name(mydf, "conf.low") && .obj_has_name(mydf, "conf.high")) {
          mydf$conf.low <- exp(exp(mydf$conf.low))
          mydf$conf.high <- exp(exp(mydf$conf.high))
        }
      } else {
        plus_minus <- eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
        if (is.null(plus_minus)) plus_minus <- 0
        mydf$predicted <- exp(mydf$predicted) - plus_minus
        if (.obj_has_name(mydf, "conf.low") && .obj_has_name(mydf, "conf.high")) {
          mydf$conf.low <- exp(mydf$conf.low) - plus_minus
          mydf$conf.high <- exp(mydf$conf.high) - plus_minus
        }
      }

      insight::format_alert("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.")
    } else {
      message("Model has log-transformed response. Predictions are on log-scale.")
    }
  }

  if (any(grepl("log1p\\((.*)\\)", rv))) {
    if (back.transform) {
      mydf$predicted <- expm1(mydf$predicted)
      if (.obj_has_name(mydf, "conf.low") && .obj_has_name(mydf, "conf.high")) {
        mydf$conf.low <- expm1(mydf$conf.low)
        mydf$conf.high <- expm1(mydf$conf.high)
      }
      insight::format_alert("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.")
    } else {
      message("Model has log-transformed response. Predictions are on log(mu + 1) scale.")
    }
  }

  if (any(grepl("sqrt\\((.*)\\)", rv))) {
    if (back.transform) {
      plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
      if (is.null(plus_minus)) plus_minus <- 0
      mydf$predicted <- mydf$predicted^2 - plus_minus
      if (.obj_has_name(mydf, "conf.low") && .obj_has_name(mydf, "conf.high")) {
        mydf$conf.low <- mydf$conf.low^2 - plus_minus
        mydf$conf.high <- mydf$conf.high^2 - plus_minus
      }
      insight::format_alert("Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.")
    } else {
      message("Model has sqrt-transformed response. Predictions are on sqrt-scale.")
    }
  }

  mydf
}
