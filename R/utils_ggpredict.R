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
      insight::format_alert("Model has log-transformed response. Predictions are on log-scale.")
    }
  }

  trans_fun <- NULL
  if (any(grepl("log1p\\((.*)\\)", rv))) {
    trans_fun <- function(x) expm1(x)
  }

  if (any(grepl("log10\\((.*)\\)", rv))) {
    trans_fun <- function(x) 10^x
  }

  if (any(grepl("log2\\((.*)\\)", rv))) {
    trans_fun <- function(x) 2^x
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
      insight::format_alert("Model has sqrt-transformed response. Predictions are on sqrt-scale.")
    }
  }

  if (!is.null(trans_fun)) {
    if (back.transform) {
      mydf$predicted <- trans_fun(mydf$predicted)
      if (.obj_has_name(mydf, "conf.low") && .obj_has_name(mydf, "conf.high")) {
        mydf$conf.low <- trans_fun(mydf$conf.low)
        mydf$conf.high <- trans_fun(mydf$conf.high)
      }
      insight::format_alert("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.")
    } else {
      insight::format_alert("Model has log-transformed response. Predictions are on log-scale.")
    }
  }

  mydf
}
