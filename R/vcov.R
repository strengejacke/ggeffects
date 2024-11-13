#' @title Calculate variance-covariance matrix for adjusted predictions
#' @name vcov
#'
#' @description Returns the variance-covariance matrix for the predicted values from `object`.
#'
#' @param object An object of class `"ggeffects"`, as returned by `predict_response()`.
#' @param ... Currently not used.
#' @inheritParams predict_response
#'
#' @return The variance-covariance matrix for the predicted values from `object`.
#'
#' @details The returned matrix has as many rows (and columns) as possible combinations
#'   of predicted values from the `predict_response()` call. For example, if there
#'   are two variables in the `terms`-argument of `predict_response()` with 3 and 4
#'   levels each, there will be 3*4 combinations of predicted values, so the returned
#'   matrix has a 12x12 dimension. In short, `nrow(object)` is always equal to
#'   `nrow(vcov(object))`. See also 'Examples'.
#'
#' @examples
#' data(efc)
#' model <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' result <- predict_response(model, c("c12hour [meansd]", "c161sex"))
#'
#' vcov(result)
#'
#' # compare standard errors
#' sqrt(diag(vcov(result)))
#' as.data.frame(result)
#'
#' # only two predicted values, no further terms
#' # vcov() returns a 2x2 matrix
#' result <- predict_response(model, "c161sex")
#' vcov(result)
#'
#' # 2 levels for c161sex multiplied by 3 levels for c172code
#' # result in 6 combinations of predicted values
#' # thus vcov() returns a 6x6 matrix
#' result <- predict_response(model, c("c161sex", "c172code"))
#' vcov(result)
#' @export
vcov.ggeffects <- function(object,
                           vcov = NULL,
                           vcov_args = NULL,
                           verbose = TRUE,
                           ...) {
  model <- .get_model_object(object)

  if (is.null(model)) {
    if (verbose) {
      insight::format_alert(
        "Can't access original model to compute variance-covariance matrix of predictions."
      )
    }
    return(NULL)
  }

  # get model data
  model_frame <- .get_model_data(model)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  random_effect_terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)


  # we can't condition on categorical variables
  condition <- attr(object, "condition")
  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- vapply(
      cn,
      function(.x) is.factor(model_frame[[.x]]) && !(.x %in% random_effect_terms),
      logical(1)
    )
    condition <- condition[!cn.factors]
    if (.is_empty(condition)) condition <- NULL
  }

  const.values <- attr(object, "constant.values")
  const.values <- c(condition, unlist(const.values[vapply(const.values, is.numeric, logical(1))]))
  original_terms <- attr(object, "original.terms")

  # copy data frame with predictions
  newdata <- .data_grid(
    model,
    model_frame,
    terms = original_terms,
    typical = "mean",
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = const.values,
    verbose = FALSE
  )

  # add response to newdata. For models fitted with "glmmPQL",
  # the response variable is renamed internally to "zz".
  if (inherits(model, "glmmPQL")) {
    new_response <- 0
    names(new_response) <- "zz"
  } else {
    fr <- insight::find_response(model, combine = FALSE)
    new_response <- rep_len(0, length(fr))
    names(new_response) <- fr
  }

  new_response <- new_response[setdiff(names(new_response), colnames(newdata))]
  newdata <- cbind(as.list(new_response), newdata)

  # clean terms from brackets
  original_terms <- .clean_terms(original_terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(original_terms) > 2) {
    trms <- original_terms[3]
    newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
  }

  if (length(original_terms) > 1) {
    trms <- original_terms[2]
    newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
  }

  trms <- original_terms[1]
  newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]

  # rownames were resorted as well, which causes troubles in model.matrix
  rownames(newdata) <- NULL
  tryCatch(
    .vcov_helper(
      model, model_frame, newdata, vcov = vcov, vcov_args = vcov_args,
      original_terms = original_terms, full.vcov = TRUE, verbose = verbose
    ),
    error = function(e) {
      if (verbose) {
        insight::format_alert(
          "Could not compute variance-covariance matrix of predictions. No confidence intervals are returned."
        )
      }
      NULL
    }
  )
}



.vcov_helper <- function(model,
                         model_frame,
                         newdata,
                         vcov,
                         vcov_args,
                         original_terms,
                         full.vcov = FALSE,
                         verbose = TRUE) {
  # get variance-covariance matrix
  vcm <- .get_variance_covariance_matrix(model, vcov, vcov_args)

  model_terms <- tryCatch(
    stats::terms(model),
    error = function(e) insight::find_formula(model)$conditional
  )

  # exception for gamlss, who may have "random()" function in formula
  # we need to remove this term...
  if (inherits(model, "gamlss") && grepl("random\\((.*\\))", insight::safe_deparse(stats::formula(model)))) {
    model_terms <- insight::find_formula(model)$conditional
  }

  # drop offset from model_terms
  if (inherits(model, c("zeroinfl", "hurdle", "zerotrunc"))) {
    all_terms <- insight::find_terms(model)$conditional
    off_terms <- grepl("^offset\\((.*)\\)", all_terms)
    if (any(off_terms)) {
      all_terms <- all_terms[!off_terms]
      ## TODO preserve interactions
      vcov_names <- dimnames(vcm)[[1]][grepl(":", dimnames(vcm)[[1]], fixed = TRUE)]
      if (length(vcov_names)) {
        vcov_names <- gsub(":", "*", vcov_names, fixed = TRUE)
        all_terms <- unique(c(all_terms, vcov_names))
      }
      off_terms <- grepl("^offset\\((.*)\\)", all_terms)
      model_terms <- stats::reformulate(all_terms[!off_terms], response = insight::find_response(model))
    }
  }

  # check if factors are held constant. if so, we have just one level in the
  # data, which is too few to compute the vcov - in this case, remove those
  # factors from model formula and vcov

  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  nlevels_terms <- vapply(
    colnames(newdata),
    function(.x) !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1,
    logical(1)
  )

  if (any(nlevels_terms)) {
    # once we have removed factors with one level only, we need to recalculate
    # the model terms
    all_terms <- setdiff(
      insight::find_terms(model)$conditional,
      colnames(newdata)[nlevels_terms]
    )
    # for the model matrix, we need the variable names, not the term notation
    # thus, we "reformulate" the terms
    model_terms <- stats::reformulate(
      all.vars(stats::reformulate(all_terms)),
      response = insight::find_response(model)
    )
    # check which terms are in the vcov-matrix, and filter
    keep_vcov_cols <- c("(Intercept)", grep(
      paste0("(", paste0("\\Q", all_terms, "\\E", collapse = "|") , ")"),
      colnames(vcm),
      value = TRUE
    ))

    insight::format_warning(paste("It happens with class", class(model)))

    keep_vcov_cols <- intersect(keep_vcov_cols, colnames(vcm))
    vcm <- vcm[keep_vcov_cols, keep_vcov_cols, drop = FALSE]
  }

  # sanity check - if "scale()" was used in formula, and that variable is
  # held constant, then scale() will return NA and no SEs are returned.
  # To check this, we extract all terms, and also save the cleaned terms as names
  # (so we can find the variables in the "newdata") - but only run this check if
  # the user wants to hear it
  if (verbose) {
    scale_terms <- insight::find_terms(model)$conditional
    scale_terms <- stats::setNames(scale_terms, insight::clean_names(scale_terms))
    # check if any term containts "scale()"
    scale_transform <- grepl("scale(", scale_terms, fixed = TRUE)
    if (any(scale_transform)) {
      # if so, kepp only terms with scale
      scale_terms <- scale_terms[scale_transform]
      # now get the data for those terms and see if any term has only 1 unique value,
      # which indicated, it is hold constant
      scale_terms <- scale_terms[names(scale_terms) %in% colnames(newdata)]
      tmp <- .safe(newdata[names(scale_terms)])
      if (!is.null(tmp)) {
        n_unique <- vapply(tmp, insight::n_unique, numeric(1))
        problematic <- n_unique == 1
        if (any(problematic)) {
          insight::format_alert(paste0(
            "`scale()` is used on the model term",
            ifelse(sum(problematic) > 1, "s ", " "),
            datawizard::text_concatenate(names(scale_terms)[problematic], enclose = "\""),
            ifelse(sum(problematic) > 1, ", which are ", ", which is "),
            "used as non-focal term and hold constant at a specific value.",
            " Confidence intervals are eventually not calculated or inaccurate.",
            " To solve this issue, standardize your variable before fitting",
            " the model and don't use `scale()` in the model-formula."
          ))
        }
      }
    }
  }

  # code to compute se of prediction taken from
  # http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions # nolint
  mm <- stats::model.matrix(model_terms, newdata)

  # here we need to fix some term names, so variable names match the column
  # names from the model matrix. NOTE that depending on the type of contrasts,
  # the naming column names for factors differs: for "contr.sum", column names
  # of factors are named "Species1", "Species2", etc., while for "contr.treatment",
  # column names are "Speciesversicolor", "Speciesvirginica", etc.

  contrs <- attr(mm, "contrasts")

  if (!.is_empty(contrs)) {
    # check which contrasts are actually in terms-argument,
    # and which terms also appear in contrasts
    keep.c <- names(contrs) %in% original_terms
    rem.t <- original_terms %in% names(contrs)

    # only iterate required terms and contrasts
    contrs <- contrs[keep.c]
    original_terms <- original_terms[!rem.t]

    add.terms <- unlist(Map(function(.x, .y) {
      f <- model_frame[[.y]]
      if (!is.factor(f)) {
        f <- factor(f, levels = sort(unique(f)))
      }
      if (.x %in% c("contr.sum", "contr.helmert")) {
        sprintf("%s%s", .y, 1:(nlevels(f) - 1))
      } else if (.x == "contr.poly") {
        sprintf("%s%s", .y, c(".L", ".Q", ".C"))
      } else {
        sprintf("%s%s", .y, levels(f)[2:nlevels(f)])
      }
    }, contrs, names(contrs)))

    original_terms <- c(original_terms, add.terms)
  }

  # we need all this intersection-stuff to reduce the model matrix and remove
  # duplicated entries. Else, especially for mixed models, we often run into
  # memory allocation problems. The problem is to find the correct rows of
  # the matrix that should be kept, and only take those columns of the
  # matrix for which terms we need standard errors.

  model_matrix_data <- as.data.frame(mm)

  # we may have backticks in our model matrix, so remove those from column names
  if (any(startsWith(colnames(model_matrix_data), "`"))) {
    colnames(model_matrix_data) <- gsub("^`(.*)`$", "\\1", colnames(model_matrix_data))
  }

  rows_to_keep <- as.numeric(rownames(unique(
    model_matrix_data[intersect(colnames(model_matrix_data), original_terms)]
  )))

  # for poly-terms, we have no match, so fix this here
  if (.is_empty(rows_to_keep) || !all(original_terms %in% colnames(model_matrix_data))) {
    inters <- which(insight::clean_names(colnames(model_matrix_data)) %in% original_terms)
    rows_to_keep <- as.numeric(rownames(unique(model_matrix_data[inters])))
  }

  mm <- mm[rows_to_keep, , drop = FALSE]

  if (inherits(model, c("polr", "mixor", "multinom", "ordinal_weightit", "brmultinom", "bracl", "fixest", "multinom_weightit"))) { # nolint
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep, drop = FALSE]
    mm <- mm[, keep]
  }

  # try to compute the variance-covariance matrix
  result <- .safe(
    if (full.vcov) {
      mm %*% vcm %*% t(mm)
    } else {
      colSums(t(mm %*% vcm) * t(mm))
    }
  )

  # sanity check for non-conformable arguments
  if (is.null(result)) {
    # make sure both matrices have the same number of columns
    shared_colums <- intersect(colnames(mm), colnames(vcm))
    mm <- mm[, shared_colums]
    vcm <- vcm[shared_colums, shared_colums, drop = FALSE]
    # try again
    result <- .safe(
      if (full.vcov) {
        mm %*% vcm %*% t(mm)
      } else {
        colSums(t(mm %*% vcm) * t(mm))
      }
    )
  }

  result
}


.get_variance_covariance_matrix <- function(model,
                                            vcov,
                                            vcov_args,
                                            skip_if_null = FALSE,
                                            verbose = TRUE) {
  # check if robust vcov-matrix is requested
  if (is.null(vcov) && skip_if_null) {
    vcm <- NULL
  } else if (!is.null(vcov)) {
    if (isTRUE(vcov)) {
      # vcov = TRUE - this is the case for `test_prediction()`, to set the
      # vcov-argument for *marginaleffects*
      vcm <- TRUE
    } else if (is.function(vcov)) {
      # user provided a function? then prepare arguments and call function
      # to return a vcov-matrix
      if (is.null(vcov_args) || !is.list(vcov_args)) {
        arguments <- list(model)
      } else {
        arguments <- c(list(model), vcov_args)
      }
      vcm <- as.matrix(do.call("vcov", arguments))
    } else if (is.matrix(vcov)) {
      # user provided a vcov-matrix? directly return in
      vcm <- as.matrix(vcov)
    } else {
      # user provided string? get the related covariance matrix estimation
      # from the *sandwich* or *clubSandwich* packages
      vcm <- as.matrix(suppressMessages(insight::get_varcov(
        model,
        vcov = vcov,
        vcov_args = vcov_args,
        component = "conditional"
      )))
    }
    # for zero-inflated models, remove zero-inflation part from vcov
    if (inherits(model, c("zeroinfl", "hurdle", "zerotrunc"))) {
      vcm <- vcm[!startsWith(rownames(vcm), "zero_"), !startsWith(rownames(vcm), "zero_"), drop = FALSE]
    }
  } else {
    # get variance-covariance-matrix, depending on model type
    vcm <- suppressMessages(insight::get_varcov(model, component = "conditional"))
  }
  vcm
}


## TODO: deprecate handling, remove later
.prepare_vcov_args <- function(vcov, ...) {
  # if user already used the vcov-argument, do nothing
  if (!is.null(vcov)) {
    return(vcov)
  }
  dots <- list(...)
  if (!is.null(dots$vcov_fun) || !is.null(dots$vcov_type)) {
    insight::format_warning("The arguments `vcov_fun` and `vcov_type` are deprecated and no longer. Please only use `vcov` to specify a variance-covariance matrix or a string to identify the function to compute heteroscedasticity-consistent standard errors, and the `vcov_args` argument for further arguments passed to that function.") # nolint
  }
  if (!is.null(dots$vcov_type)) {
    return(dots$vcov_type)
  }
  if (!is.null(dots$vcov_fun) && (is.matrix(dots$vcov_fun) || is.function(dots$vcov_fun))) {
    return(dots$vcov_fun)
  }
  vcov_fun <- dots$vcov_fun
  if (!is.null(vcov_fun) && startsWith(vcov_fun, "vcov")) {
    vcov_fun <- gsub("^vcov(.*)", "\\1", vcov_fun)
  }
  vcov_fun
}
