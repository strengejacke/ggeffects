#' @importFrom insight find_variables
.back_transform_response <- function(model, mydf, back.transform) {
  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale

  rv <- insight::find_variables(model)[["response"]]

  if (back.transform && any(grepl("log\\((.*)\\)", rv))) {

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
  }

  mydf
}
