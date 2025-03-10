.validate_type_argument <- function(model,
                                    type,
                                    marginaleffects = FALSE,
                                    emmeans_call = FALSE) {
  # marginaleffects supports the predict-method types
  # we need a different approach to validation here
  if (marginaleffects) {
    # for zero-inflation models, we need to find the correct name
    # for the type argument...
    is_zero_inflated <- insight::model_info(model)$is_zero_inflated
    if (is_zero_inflated) {
      if (inherits(model, "glmmTMB")) {
        types <- c("conditional", "zprob")
      } else {
        types <- c("count", "zero")
      }
    }
    # first, we overwrite the "default"
    if (type == "fixed") {
      if (is_zero_inflated) {
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
    return(type)
  }

  # if we call "predict()" or "emmeans()", we have these different options
  if (emmeans_call) {
    type_choices <- c("fixed", "count", "zero_inflated", "zi_prob")
  } else {
    type_choices <- c(
      "fixed", "count", "random", "zero_inflated", "zi_random",
      "zero_inflated_random", "zi_prob", "simulate", "survival",
      "cumulative_hazard", "simulate_random", "debug", "quantile" # for survreg
    )
  }
  type <- insight::validate_argument(type, type_choices)

  switch(type,
    count = "fixed",
    zi_random = "zero_inflated_random",
    type
  )
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
