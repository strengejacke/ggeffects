.validate_type_argument <- function(model, type, ppd, marginaleffects = FALSE) {
  # marginaleffects supports the predict-method types
  # we need a different approach to validation here
  if (marginaleffects) {
    # for zero-inflation models, we need to find the correct name
    # for the type argument...
    if (inherits(model, c("glmmTMB", "zeroinfl", "hurdle"))) {
      if (inherits(model, "glmmTMB")) {
        types <- c("conditional", "zprob")
      } else {
        types <- c("count", "zero")
      }
    }
    # first, we overwrite the "default"
    if (type == "fixed") {
      if (inherits(model, c("glmmTMB", "zeroinfl", "hurdle"))) {
        type <- types[1]
      } else if (class(model)[1] %in% .default_type$class) {
        type <- .default_type$type[.default_type$class == class(model)[1]]
      } else {
        type <- "response"
      }
    } else if (type %in% c("zi", "zero_inflated", "fe.zi")) {
      type <- "response"
    } else if (type %in% c("zi.prob", "zi_prob")) {
      type <- types[2]
    }
    # check which types are supported by the model's predict-method
    type_options <- .typedic$type[.typedic$class == class(model)[1]]
    if (!type %in% c("response", type_options)) {
      insight::format_error(sprintf(
        "`type = \"%s\"` is not supported. Please use %s%s.",
        type,
        if (length(type_options) > 1) "one of " else "",
        toString(paste0("`", type_options, "`"))
      ))
    }
    return(list(type = type, ppd = ppd))
  }

  # if we call "predict()" or "emmeans()", we have these different options
  type <- match.arg(type, choices = c(
    "fe", "fixed", "count", "re", "random",
    "fe.zi", "zero_inflated", "re.zi", "zi_random",
    "zero_inflated_random", "zi.prob", "zi_prob",
    "sim", "simulate", "surv", "survival", "cumhaz",
    "cumulative_hazard", "sim_re", "simulate_random",
    "debug", "fixed_ppd", "random_ppd"
  ))
  # handle Bayes exceptions for type with ppd
  if (type %in% c("fixed_ppd", "random_ppd")) {
    ppd <- TRUE
    type <- gsub("_ppd", "", type, fixed = TRUE)
  }

  type <- switch(type,
    fixed = ,
    count = "fe",
    random = "re",
    zi = ,
    zero_inflated = "fe.zi",
    zi_random = ,
    zero_inflated_random = "re.zi",
    zi_prob = "zi.prob",
    survival = "surv",
    cumulative_hazard = "cumhaz",
    simulate = "sim",
    simulate_random = "sim_re",
    type
  )

  list(type = type, ppd = ppd)
}


.retrieve_type_option <- function(model) {
  # retrieve model object's predict-method prediction-types (if any)
  predict_method <- .safe(lapply(
    class(model), function(i) {
      utils::getS3method("predict", i)
    }
  ))
  # check whether model class has a predict method
  if (!is.null(predict_method)) {
    predict_method <- predict_method[!vapply(predict_method, is.null, TRUE)][[1]]
  }
  # retrieve model object's predict-method prediction-types (if any)
  .safe(suppressWarnings(eval(formals(predict_method)$type)))
}


.back_transform_response <- function(model, mydf, back.transform, response.name = NULL, verbose = TRUE) {
  # skip if no information available
  if (is.null(model) && is.null(response.name)) {
    return(mydf)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  if (is.null(response.name)) {
    rv <- insight::find_terms(model)[["response"]]
  } else {
    rv <- response.name
  }

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
      if (verbose) {
        insight::format_alert("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.") # nolint
      }
    } else if (verbose) {
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
      if (verbose) {
        insight::format_alert("Model has sqrt-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the sqrt-scale.") # nolint
      }
    } else if (verbose) {
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
      if (verbose) {
        insight::format_alert("Model has log-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the log-scale.") # nolint
      }
    } else if (verbose) {
      insight::format_alert("Model has log-transformed response. Predictions are on log-scale.")
    }
  }

  mydf
}


.back_transform_data <- function(model, mydf, back.transform, response.name = NULL) {
  # skip if no information available
  if (is.null(mydf)) {
    return(NULL)
  }
  if (back.transform) {
    return(mydf)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  if (is.null(response.name)) {
    rv <- insight::find_terms(model)[["response"]]
  } else {
    rv <- response.name
  }

  # sanity check
  if (!"response" %in% colnames(mydf)) {
    return(mydf)
  }

  if (any(grepl("log\\((.*)\\)", rv))) {
    # do we have log-log models?
    if (grepl("log\\(log\\((.*)\\)\\)", rv)) {
      mydf$response <- log(log(mydf$response))
    } else {
      plus_minus <- eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
      if (is.null(plus_minus)) plus_minus <- 0
      mydf$response <- log(mydf$response) + plus_minus
    }
  }

  trans_fun <- NULL
  if (any(grepl("log1p\\((.*)\\)", rv))) {
    trans_fun <- function(x) log1p(x)
  }

  if (any(grepl("log10\\((.*)\\)", rv))) {
    trans_fun <- function(x) log10(x)
  }

  if (any(grepl("log2\\((.*)\\)", rv))) {
    trans_fun <- function(x) log2(x)
  }

  if (any(grepl("sqrt\\((.*)\\)", rv))) {
    plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
    if (is.null(plus_minus)) plus_minus <- 0
    mydf$response <- sqrt(mydf$response) + plus_minus
  }

  if (!is.null(trans_fun)) {
    mydf$response <- trans_fun(mydf$response)
  }

  mydf
}
