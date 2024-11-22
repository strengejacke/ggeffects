.back_transform_response <- function(model, mydf, back_transform, response.name = NULL, verbose = TRUE) {
  # skip if no information available
  if (is.null(model) && is.null(response.name)) {
    return(mydf)
  }

  # skip for multivariate response models
  if (insight::is_multivariate(model)) {
    # tell user
    if (verbose) {
      insight::format_alert("Back-transforming response variables is not carried out for multivariate-response models.")
    }
    return(mydf)
  }

  # we need the string of the response variable, to get information about transformation
  if (is.null(response.name)) {
    rv <- insight::find_terms(model, verbose = FALSE)[["response"]]
  } else {
    # for pool_predictions(), we have no model object, but the response-string
    rv <- response.name
  }

  # for pool_predictions(), we have no model object, but rather the response-string
  if (is.null(model)) {
    # find possible transformations from response-string
    transformation <- insight::find_transformation(rv)
  } else {
    # find possible transformations from model
    transformation <- insight::find_transformation(model)
  }

  # skip if no information available, or no transformation applies
  if (is.null(transformation) || identical(transformation, "identity")) {
    return(mydf)
  }

  # transformed response, but no back-transform requested?
  # Tell user and return untransformed predictions
  if (!back_transform) {
    if (verbose) {
      insight::format_alert(
        paste0("Model has ", transformation, " transformed response. Predictions are on transformed scale.")
      )
    }
    return(mydf)
  }

  # for pool_predictions(), we have no model object, but rather the response-string
  if (is.null(model)) {
    # get inverse transformation function response-string
    trans_fun <- insight::get_transformation(rv, verbose = verbose)$inverse
  } else {
    # get inverse transformation function from model
    trans_fun <- insight::get_transformation(model, verbose = verbose)$inverse
  }

  # Tell user about transformation
  if (verbose && !is.null(trans_fun)) {
    insight::format_alert(
      paste0("Model has ", transformation, "-transformed response. Back-transforming predictions to original response scale. Standard errors are still on the transformed scale.") # nolint
    )
  }

  if (startsWith(transformation, "sqrt")) {
    # handle sqrt-transformed response separately - might be "sqrt(x + 1)"
    plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
    if (is.null(plus_minus)) plus_minus <- 0
    mydf$predicted <- mydf$predicted^2 - plus_minus
    if (all(c("conf.low", "conf.high") %in% colnames(mydf))) {
      mydf$conf.low <- mydf$conf.low^2 - plus_minus
      mydf$conf.high <- mydf$conf.high^2 - plus_minus
    }
  } else if (startsWith(transformation, "log(") && transformation != "log-log") {
    # handle log-transformed response separately - might be "log(x + 1)"
    plus_minus <- eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
    if (is.null(plus_minus)) plus_minus <- 0
    mydf$predicted <- exp(mydf$predicted) - plus_minus
    if (all(c("conf.low", "conf.high") %in% colnames(mydf))) {
      mydf$conf.low <- exp(mydf$conf.low) - plus_minus
      mydf$conf.high <- exp(mydf$conf.high) - plus_minus
    }
  } else if (!is.null(trans_fun)) {
    mydf$predicted <- trans_fun(mydf$predicted)
    if (all(c("conf.low", "conf.high") %in% colnames(mydf))) {
      mydf$conf.low <- trans_fun(mydf$conf.low)
      mydf$conf.high <- trans_fun(mydf$conf.high)
    }
  }

  mydf
}


.back_transform_data <- function(model, mydf, back_transform, response.name = NULL) {
  # skip if no information available
  if (is.null(mydf)) {
    return(NULL)
  }
  if (back_transform) {
    return(mydf)
  }

  # check if outcome is log-transformed, and if so,
  # back-transform predicted values to response scale
  if (is.null(response.name)) {
    rv <- insight::find_terms(model, verbose = FALSE)[["response"]]
  } else {
    rv <- response.name
  }

  # sanity check
  if (!"response" %in% colnames(mydf)) {
    return(mydf)
  }

  # find transformation
  if (is.null(model)) {
    # find possible transformations from response-string
    transformation <- insight::find_transformation(rv)
  } else {
    # find possible transformations from model
    transformation <- insight::find_transformation(model)
  }

  # get transformation function
  if (is.null(model)) {
    # get inverse transformation function response-string
    trans_fun <- insight::get_transformation(rv, verbose = FALSE)$transformation
  } else {
    # get inverse transformation function from model
    trans_fun <- insight::get_transformation(model, verbose = FALSE)$transformation
  }

  if (!is.null(transformation) && !identical(transformation, "identity") && !is.null(trans_fun)) {
    if (startsWith(transformation, "sqrt")) {
      # handle sqrt-transformed response separately - might be "sqrt(x + 1)"
      plus_minus <- eval(parse(text = gsub("sqrt\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
      if (is.null(plus_minus)) plus_minus <- 0
      mydf$response <- sqrt(mydf$response) + plus_minus
    } else if (startsWith(transformation, "log(") && transformation != "log-log") {
      # handle log-transformed response separately - might be "log(x + 1)"
      plus_minus <- eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", rv)))
      if (is.null(plus_minus)) plus_minus <- 0
      mydf$response <- log(mydf$response) + plus_minus
    } else if (!is.null(trans_fun)) {
      mydf$response <- trans_fun(mydf$response)
    }
  }

  mydf
}


.check_model_object <- function(model) {
  # tidymodels?
  if (inherits(model, "model_fit")) {
    model <- model$fit
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the gam-part then
  if (is.gamm(model) || is.gamm4(model)) {
    model <- model$gam
  }

  # for sdmTMB objects, delta/hurdle models have family lists
  if (.is_delta_sdmTMB(model)) {
    insight::format_error("`ggpredict()` does not yet work with `sdmTMB` delta models.")
  }

  model
}
