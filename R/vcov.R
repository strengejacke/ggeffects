#' @title Calculate variance-covariance matrix for marginal effects
#' @name vcov
#'
#' @description Returns the variance-covariance matrix for the predicted values from \code{object}.
#'
#' @param object An object of class \code{"ggeffects"}, as returned by \code{ggpredict()}.
#' @param ... Currently not used.
#' @inheritParams ggpredict
#'
#' @return The variance-covariance matrix for the predicted values from \code{object}.
#'
#' @details The returned matrix has as many rows (and columns) as possible combinations
#'   of predicted values from the \code{ggpredict()} call. For example, if there
#'   are two variables in the \code{terms}-argument of \code{ggpredict()} with 3 and 4
#'   levels each, there will be 3*4 combinations of predicted values, so the returned
#'   matrix has a 12x12 dimension. In short, \code{nrow(object)} is always equal to
#'   \code{nrow(vcov(object))}. See also 'Examples'.
#'
#' @examples
#' data(efc)
#' model <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#' result <- ggpredict(model, c("c12hour [meansd]", "c161sex"))
#'
#' vcov(result)
#'
#' # compare standard errors
#' sqrt(diag(vcov(result)))
#' as.data.frame(result)
#'
#' # only two predicted values, no further terms
#' # vcov() returns a 2x2 matrix
#' result <- ggpredict(model, "c161sex")
#' vcov(result)
#'
#' # 2 levels for c161sex multiplied by 3 levels for c172code
#' # result in 6 combinations of predicted values
#' # thus vcov() returns a 6x6 matrix
#' result <- ggpredict(model, c("c161sex", "c172code"))
#' vcov(result)
#' @export
vcov.ggeffects <- function(object, vcov.fun = NULL, vcov.type = NULL, vcov.args = NULL, ...) {
  model <- tryCatch(get(attr(object, "model.name"), envir = parent.frame()),
                    error = function(e) NULL)

  if (is.null(model)) {
    warning(insight::format_message(
      "Can't access original model to compute variance-covariance matrix of predictions."
    ), call. = FALSE)
    return(NULL)
  }

  model_frame <- insight::get_data(model)

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
    cn.factors <- sapply(cn, function(.x) is.factor(model_frame[[.x]]) && !(.x %in% random_effect_terms))
    condition <- condition[!cn.factors]
    if (.is_empty(condition)) condition <- NULL
  }

  const.values <- attr(object, "constant.values")
  const.values <- c(condition, unlist(const.values[sapply(const.values, is.numeric)]))
  terms <- attr(object, "original.terms")


  ## TODO fpr debugging
  add.args <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if (isTRUE(add.args[["debug"]])) {
    message("Collection 1")
    print(gc(TRUE))
  }

  # copy data frame with predictions
  newdata <- .data_grid(
    model,
    model_frame,
    terms,
    value_adjustment = "mean",
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = const.values
  )


  ## TODO fpr debugging
  if (isTRUE(add.args[["debug"]])) {
    message("Collection 2")
    print(gc(TRUE))
  }


  # add response to newdata. For models fitted with "glmmPQL",
  # the response variable is renamed internally to "zz".

  if (inherits(model, "glmmPQL")) {
    new_response <- 0
    names(new_response) <- "zz"
  } else {
    fr <- insight::find_response(model, combine = FALSE)
    new_response <- rep(0, length.out = length(fr))
    names(new_response) <- fr
  }

  new_response <- new_response[setdiff(names(new_response), colnames(newdata))]
  newdata <- cbind(as.list(new_response), newdata)

  # clean terms from brackets
  terms <- .clean_terms(terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(terms) > 2) {
    trms <- terms[3]
    newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
  }

  if (length(terms) > 1) {
    trms <- terms[2]
    newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]
  }

  trms <- terms[1]
  newdata <- newdata[order(newdata[[trms]]), , drop = FALSE]

  # rownames were resorted as well, which causes troubles in model.matrix
  rownames(newdata) <- NULL
  tryCatch(
    {
      .vcov_helper(
        model, model_frame, get_predict_function(model), newdata,
        vcov.fun, vcov.type, vcov.args, terms, full.vcov = TRUE
      )
    },
    error = function(e) {
      message(insight::format_message(
        "Could not compute variance-covariance matrix of predictions. No confidence intervals are returned."
      ))
      NULL
    }
  )
}



.vcov_helper <- function(model, model_frame, model_class, newdata, vcov.fun, vcov.type, vcov.args, terms, full.vcov = FALSE) {
  # check if robust vcov-matrix is requested
  if (!is.null(vcov.fun)) {
    # check for existing vcov-prefix
    if (!grepl("^vcov", vcov.fun)) {
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
    # compute robust standard errors based on vcov
    if (robust_package == "sandwich") {
      vcov.fun <- get(vcov.fun, asNamespace("sandwich"))
      vcm <- as.matrix(do.call(vcov.fun, c(list(x = model, type = vcov.type), vcov.args)))
    } else {
      vcov.fun <- clubSandwich::vcovCR
      vcm <- as.matrix(do.call(vcov.fun, c(list(obj = model, type = vcov.type), vcov.args)))
    }
  } else {
    # get variance-covariance-matrix, depending on model type
    vcm <- insight::get_varcov(model, component = "conditional")
  }


  model_terms <- tryCatch({
    stats::terms(model)
  },
  error = function(e) {
    insight::find_formula(model)$conditional
  })

  # exception for gamlss, who may have "random()" function in formula
  # we need to remove this term...
  if (inherits(model, "gamlss") && grepl("random\\((.*\\))", .safe_deparse(stats::formula(model)))) {
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

  nlevels_terms <- sapply(
    colnames(newdata),
    function(.x) !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1
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
  # http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
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
    keep.c <- names(contrs) %in% terms
    rem.t <- terms %in% names(contrs)

    # only iterate required terms and contrasts
    contrs <- contrs[keep.c]
    terms <- terms[!rem.t]

    add.terms <- unlist(mapply(function(.x, .y) {
      f <- model_frame[[.y]]
      if (!is.factor(f)) {
        f <- as.factor(f)
      }
      if (.x %in% c("contr.sum", "contr.helmert"))
        sprintf("%s%s", .y, 1:(nlevels(f) - 1))
      else if (.x == "contr.poly")
        sprintf("%s%s", .y, c(".L", ".Q", ".C"))
      else
        sprintf("%s%s", .y, levels(f)[2:nlevels(f)])
    }, contrs, names(contrs), SIMPLIFY = FALSE))

    terms <- c(terms, add.terms)
  }

  # we need all this intersection-stuff to reduce the model matrix and remove
  # duplicated entries. Else, especially for mixed models, we often run into
  # memory allocation problems. The problem is to find the correct rows of
  # the matrix that should be kept, and only take those columns of the
  # matrix for which terms we need standard errors.

  model_matrix_data <- as.data.frame(mm)
  rows_to_keep <- as.numeric(rownames(unique(model_matrix_data[intersect(colnames(model_matrix_data), terms)])))

  # for poly-terms, we have no match, so fix this here
  if (.is_empty(rows_to_keep) || !all(terms %in% colnames(model_matrix_data))) {
    inters <- which(insight::clean_names(colnames(model_matrix_data)) %in% terms)
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
