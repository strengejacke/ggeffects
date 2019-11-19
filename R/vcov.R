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
#'
#' @importFrom stats model.matrix terms formula
#' @importFrom purrr map flatten_chr map_lgl map2
#' @importFrom sjmisc is_empty
#' @importFrom insight find_random clean_names find_parameters get_varcov
#' @export
vcov.ggeffects <- function(object, vcov.fun = NULL, vcov.type = NULL, vcov.args = NULL, ...) {
  model <- tryCatch({
      get(attr(object, "model.name"), envir = parent.frame())
    },
    error = function(e) { NULL }
    )

  if (is.null(model)) {
    warning("Can't access original model to compute variance-covariance matrix of predictions.", call. = FALSE)
    return(NULL)
  }

  mf <- insight::get_data(model)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)


  # we can't condition on categorical variables
  condition <- attr(object, "condition")
  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- purrr::map_lgl(cn, ~ is.factor(mf[[.x]]) && !(.x %in% re.terms))
    condition <- condition[!cn.factors]
    if (sjmisc::is_empty(condition)) condition <- NULL
  }

  const.values <- attr(object, "constant.values")
  const.values <- c(condition, unlist(const.values[sapply(const.values, is.numeric)]))
  terms <- attr(object, "original.terms")

  # copy data frame with predictions
  newdata <- .get_data_grid(
    model,
    mf,
    terms,
    typ.fun = "mean",
    fac.typical = FALSE,
    pretty.message = FALSE,
    condition = const.values
  )

  # make sure we have enough values to compute CI
  nlevels_terms <- purrr::map_lgl(
    colnames(newdata),
    ~ !(.x %in% re.terms) &&
      is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1
  )

  if (any(nlevels_terms)) {
    not_enough <- colnames(newdata)[which(nlevels_terms)[1]]
    remove_lvl <- paste0("[", gsub(pattern = "(.*)\\[(.*)\\]", replacement = "\\2", x = terms[which(.get_cleaned_terms(terms) == not_enough)]), "]", collapse = "")
    stop(sprintf("`%s` does not have enough factor levels. Try to remove `%s`.", not_enough, remove_lvl), call. = TRUE)
  }


  # add response to newdata. For models fitted with "glmmPQL",
  # the response variable is renamed internally to "zz".

  if (inherits(model, "glmmPQL")) {
    new.resp <- 0
    names(new.resp) <- "zz"
  } else {
    fr <- insight::find_response(model, combine = FALSE)
    new.resp <- rep(0, length.out = length(fr))
    names(new.resp) <- fr
  }

  new.resp <- new.resp[setdiff(names(new.resp), colnames(newdata))]
  newdata <- sjmisc::add_variables(newdata, as.list(new.resp), .after = -1)

  # clean terms from brackets
  terms <- .get_cleaned_terms(terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(terms) > 2) {
    trms <- terms[3]
    newdata <- newdata[order(newdata[[trms]]), ]
  }

  if (length(terms) > 1) {
    trms <- terms[2]
    newdata <- newdata[order(newdata[[trms]]), ]
  }

  trms <- terms[1]
  newdata <- newdata[order(newdata[[trms]]), ]

  # rownames were resorted as well, which causes troubles in model.matrix
  rownames(newdata) <- NULL

  # check if robust vcov-matrix is requested
  if (!is.null(vcov.fun)) {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package `sandwich` needed for this function. Please install and try again.")
    }
    vcov.fun <- get(vcov.fun, asNamespace("sandwich"))
    vcm <- as.matrix(do.call(vcov.fun, c(list(x = model, type = vcov.type), vcov.args)))
  } else {
    # get variance-covariance-matrix, depending on model type
    vcm <- insight::get_varcov(model, component = "conditional")
  }


  # code to compute se of prediction taken from
  # http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions
  mm <- stats::model.matrix(stats::terms(model), newdata)

  # here we need to fix some term names, so variable names match the column
  # names from the model matrix. NOTE that depending on the type of contrasts,
  # the naming column names for factors differs: for "contr.sum", column names
  # of factors are named "Species1", "Species2", etc., while for "contr.treatment",
  # column names are "Speciesversicolor", "Speciesvirginica", etc.

  contrs <- attr(mm, "contrasts")

  if (!sjmisc::is_empty(contrs)) {

    # check which contrasts are actually in terms-argument,
    # and which terms also appear in contrasts
    keep.c <- names(contrs) %in% terms
    rem.t <- terms %in% names(contrs)

    # only iterate required terms and contrasts
    contrs <- contrs[keep.c]
    terms <- terms[!rem.t]

    add.terms <- purrr::map2(contrs, names(contrs), function(.x, .y) {
      f <- mf[[.y]]
      if (.x %in% c("contr.sum", "contr.helmert"))
        sprintf("%s%s", .y, 1:(nlevels(f) - 1))
      else if (.x == "contr.poly")
        sprintf("%s%s", .y, c(".L", ".Q", ".C"))
      else
        sprintf("%s%s", .y, levels(f)[2:nlevels(f)])
    }) %>%
      purrr::flatten_chr()

    terms <- c(terms, add.terms)
  }


  # we need all this intersection-stuff to reduce the model matrix and remove
  # duplicated entries. Else, especially for mixed models, we often run into
  # memory allocation problems. The problem is to find the correct rows of
  # the matrix that should be kept, and only take those columns of the
  # matrix for which terms we need standard errors.

  mmdf <- as.data.frame(mm)
  mm.rows <- as.numeric(rownames(unique(mmdf[intersect(colnames(mmdf), terms)])))

  # for poly-terms, we have no match, so fix this here
  if (sjmisc::is_empty(mm.rows) || !all(terms %in% colnames(mmdf))) {
    inters <- which(insight::clean_names(colnames(mmdf)) %in% terms)
    mm.rows <- as.numeric(rownames(unique(mmdf[inters])))
  }

  mm <- mm[mm.rows, ]

  # check class of fitted model, to make sure we have just one class-attribute
  # (while "inherits()" may return multiple attributes)
  model_class <- get_predict_function(model)

  if (!is.null(model_class) && model_class %in% c("polr", "multinom", "brmultinom", "bracl")) {
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep]
    mm <- mm[, keep]
  }

  mm %*% vcm %*% t(mm)
}
