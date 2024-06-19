#' @title Calculate variance-covariance matrix for adjusted predictions
#' @name vcov
#'
#' @description Returns the variance-covariance matrix for the predicted values from `object`.
#'
#' @param object An object of class `"ggeffects"`, as returned by `predict_response()`.
#' @param ... Currently not used.
#' @inheritParams predict_response
#' @param vcov.fun,vcov.type,vcov.args Deprecated. Use `vcov_fun`, `vcov_type`
#' and `vcov_args` instead.
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
                           vcov_fun = NULL,
                           vcov_type = NULL,
                           vcov_args = NULL,
                           vcov.fun = vcov_fun,
                           vcov.type = vcov_type,
                           vcov.args = vcov_args,
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
    value_adjustment = "mean",
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
      model, model_frame, get_predict_function(model), newdata,
      vcov.fun = vcov_fun, vcov.type = vcov_type, vcov.args = vcov_args,
      original_terms = original_terms, full.vcov = TRUE
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
                         model_class,
                         newdata,
                         vcov.fun,
                         vcov.type,
                         vcov.args,
                         original_terms,
                         full.vcov = FALSE) {
  # get variance-covariance matrix
  vcm <- .get_variance_covariance_matrix(model, vcov.fun, vcov.args, vcov.type)

  model_terms <- tryCatch(
    stats::terms(model),
    error = function(e) insight::find_formula(model)$conditional
  )

  # exception for gamlss, who may have "random()" function in formula
  # we need to remove this term...
  if (inherits(model, "gamlss") && grepl("random\\((.*\\))", insight::safe_deparse(stats::formula(model)))) {
    model_terms <- insight::find_formula(model)$conditional
  }

  # drop offset from model_terms+
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

  # check if factors are held constant. if so, we have just one
  # level in the data, which is too few to compute the vcov -
  # in this case, remove those factors from model formula and vcov

  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

  nlevels_terms <- vapply(
    colnames(newdata),
    function(.x) !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1,
    logical(1)
  )

  if (any(nlevels_terms)) {
    all_terms <- setdiff(
      insight::find_terms(model)$conditional,
      colnames(newdata)[nlevels_terms]
    )
    model_terms <- stats::reformulate(all_terms, response = insight::find_response(model))
    vcm <- vcm[!nlevels_terms, !nlevels_terms, drop = FALSE]
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
        f <- factor(f, levels = unique(f))
      }
      if (.x %in% c("contr.sum", "contr.helmert"))
        sprintf("%s%s", .y, 1:(nlevels(f) - 1))
      else if (.x == "contr.poly")
        sprintf("%s%s", .y, c(".L", ".Q", ".C"))
      else
        sprintf("%s%s", .y, levels(f)[2:nlevels(f)])
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

  # check class of fitted model, to make sure we have just one class-attribute
  # (while "inherits()" may return multiple attributes)
  model_class <- get_predict_function(model)

  if (!is.null(model_class) && model_class %in% c("polr", "mixor", "multinom", "brmultinom", "bracl", "fixest")) {
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep, drop = FALSE]
    mm <- mm[, keep]
  }

  if (full.vcov) {
    mm %*% vcm %*% t(mm)
  } else {
    colSums(t(mm %*% vcm) * t(mm))
  }
}


.get_variance_covariance_matrix <- function(model,
                                            vcov.fun,
                                            vcov.args,
                                            vcov.type,
                                            skip_if_null = FALSE,
                                            verbose = TRUE) {
  # check if robust vcov-matrix is requested
  if (is.null(vcov.fun) && skip_if_null) {
    vcm <- NULL
  } else if (!is.null(vcov.fun)) {
    # user provided a function?
    if (is.function(vcov.fun)) {
      if (is.null(vcov.args) || !is.list(vcov.args)) {
        arguments <- list(model)
      } else {
        arguments <- c(list(model), vcov.args)
      }
      vcm <- as.matrix(do.call("vcov.fun", arguments))
    } else if (is.matrix(vcov.fun)) {
      # user provided a vcov-matrix?
      vcm <- as.matrix(vcov.fun)
    } else {
      vcov_info <- .prepare_vcov_args(model, vcov.fun, vcov.type, vcov.args, verbose = verbose)
      vcm <- as.matrix(do.call(vcov_info$vcov.fun, vcov_info$vcov.args))
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


.prepare_vcov_args <- function(model,
                               vcov.fun,
                               vcov.type,
                               vcov.args,
                               include_model = TRUE,
                               verbose = TRUE) {
  # check for existing vcov-prefix
  if (!startsWith(vcov.fun, "vcov")) {
    vcov_shortcuts <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC5", "HC4m",
                        "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")
    # check whether a "type" is provided in vcov.fun
    if (is.null(vcov.type) && vcov.fun %in% vcov_shortcuts) {
      vcov.type <- vcov.fun
      if (startsWith(vcov.fun, "HC")) {
        vcov.fun <- "HC"
      } else {
        vcov.fun <- "CR"
      }
    }
    vcov.fun <- paste0("vcov", vcov.fun)
  }
  # set default for clubSandwich
  if (vcov.fun == "vcovCR" && is.null(vcov.type)) {
    vcov.type <- "CR0"
  }
  if (!is.null(vcov.type) && vcov.type %in% c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")) {
    insight::check_if_installed("clubSandwich")
    robust_package <- "clubSandwich"
    vcov.fun <- "vcovCR"
  } else {
    insight::check_if_installed("sandwich")
    robust_package <- "sandwich"
  }
  # clubSandwich does not work for pscl models
  if (robust_package == "clubSandwich" && inherits(model, c("zeroinfl", "hurdle", "zerotrunc"))) {
    if (verbose) {
      insight::format_alert(paste0(
        "Can't compute robust standard errors for models of class `",
        class(model)[1],
        "` when `vcov_fun=\"vcovCR\". Please use `vcov_fun=\"vcovCL\"` from the {sandwich} package instead."
      ))
    }
    return(NULL)
  }
  # compute robust standard errors based on vcov
  if (robust_package == "sandwich") {
    vcov.fun <- get(vcov.fun, asNamespace("sandwich"))
  } else {
    vcov.fun <- clubSandwich::vcovCR
  }

  if (include_model) {
    list(vcov.fun = vcov.fun, vcov.args = c(list(model, type = vcov.type), vcov.args))
  } else {
    list(vcov.fun = vcov.fun, vcov.args = c(list(type = vcov.type), vcov.args))
  }
}
