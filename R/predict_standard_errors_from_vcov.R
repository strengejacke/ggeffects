# get standard errors of predictions from model matrix and vcov ----

get_se_from_vcov <- function(model,
                             fitfram,
                             typical,
                             terms,
                             fun = NULL,
                             type = "fe",
                             vcov.fun = NULL,
                             vcov.type = NULL,
                             vcov.args = NULL,
                             condition = NULL) {

  se <- tryCatch(
    {
      safe_se_from_vcov(
        model,
        fitfram,
        typical,
        terms,
        fun,
        type,
        vcov.fun,
        vcov.type,
        vcov.args,
        condition
      )
    },
    error = function(x) { NULL },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  if (is.null(se))
    message(sprintf("Could not compute confidence intervals, probably due to memory allocation problems. Try using the `n`-tag (with different values) to reduce number of predicted values, e.g. `terms=\"%s [n=10]\"`.", terms[1]))

  se
}

#' @importFrom stats model.matrix terms vcov formula
#' @importFrom dplyr arrange n_distinct
#' @importFrom sjstats resp_var model_frame var_names
#' @importFrom rlang parse_expr
#' @importFrom purrr map flatten_chr map_lgl map2
#' @importFrom sjmisc is_empty
#' @importFrom lme4 VarCorr
safe_se_from_vcov <- function(model,
                              fitfram,
                              typical,
                              terms,
                              fun,
                              type,
                              vcov.fun,
                              vcov.type,
                              vcov.args,
                              condition) {

  mf <- sjstats::model_frame(model, fe.only = FALSE)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  re.terms <- sjstats::re_grp_var(model)


  # we can't condition on categorical variables

  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- purrr::map_lgl(cn, ~ is.factor(mf[[.x]]) && !(.x %in% re.terms))
    condition <- condition[!cn.factors]
    if (sjmisc::is_empty(condition)) condition <- NULL
  }


  # copy data frame with predictions
  newdata <- get_expanded_data(
    model,
    mf,
    terms,
    typ.fun = typical,
    fac.typical = FALSE,
    pretty.message = FALSE,
    condition = condition
  )

  # make sure we have enough values to compute CI
  if (any(purrr::map_lgl(colnames(newdata), ~ !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1)))
    return(NULL)


  # add response to newdata. in case we have a matrix as outcome
  # (when using "cbind()"), we need to add both variables here.
  # For models fitted with "glmmPQL", the response variable is
  # renamed internally to "zz".

  if (inherits(model, "glmmPQL"))
    new.resp <- "zz"
  else
    new.resp <- sjstats::var_names(sjstats::resp_var(model))


  if (!sjmisc::is_empty(string_starts_with(pattern = "cbind(", x = new.resp))) {
    av <- all.vars(stats::formula(model))
    get.cb <- purrr::map_lgl(av, ~ grepl(.x, new.resp, fixed = T))
    new.resp <- av[get.cb]
    newdata <- sjmisc::add_variables(
      newdata,
      data.frame(
        response.val1 = 0,
        response.val2 = 0
      ),
      .after = -1
    )
    colnames(newdata)[1:2] <- new.resp
  } else {
    newdata <- sjmisc::add_variables(newdata, data.frame(response.val = 0), .after = -1)
    # proper column names, needed for getting model matrix
    colnames(newdata)[1] <- new.resp
  }


  # clean terms from brackets
  terms <- get_clear_vars(terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(terms) > 2) {
    trms <- rlang::parse_expr(terms[3])
    newdata <- dplyr::arrange(newdata, !! trms)
    fitfram <- dplyr::arrange(fitfram, !! trms)
  }

  if (length(terms) > 1) {
    trms <- rlang::parse_expr(terms[2])
    newdata <- dplyr::arrange(newdata, !! trms)
    fitfram <- dplyr::arrange(fitfram, !! trms)
  }

  trms <- rlang::parse_expr(terms[1])
  newdata <- dplyr::arrange(newdata, !! trms)
  fitfram <- dplyr::arrange(fitfram, !! trms)


  # check if robust vcov-matrix is requested
  if (!is.null(vcov.fun)) {
    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("Package `sandwich` needed for this function. Please install and try again.")
    }
    vcov.fun <- get(vcov.fun, asNamespace("sandwich"))
    vcm <- as.matrix(do.call(vcov.fun, c(list(x = model, type = vcov.type), vcov.args)))
  } else {
    # get variance-covariance-matrix, depending on model type
    if (is.null(fun))
      vcm <- as.matrix(stats::vcov(model))
    else if (fun %in% c("hurdle", "zeroinfl", "zerotrunc")) {
      vcm <- as.matrix(stats::vcov(model, model = "count"))
    } else if (fun == "betareg")
      vcm <- as.matrix(stats::vcov(model, model = "mean"))
    else if (fun == "truncreg") {
      vcm <- as.matrix(stats::vcov(model))
      # remove sigma from matrix
      vcm <- vcm[1:(nrow(vcm) - 1), 1:(ncol(vcm) - 1)]
    } else
      vcm <- as.matrix(stats::vcov(model))
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
    inters <- which(sjstats::var_names(colnames(mmdf)) %in% terms)
    mm.rows <- as.numeric(rownames(unique(mmdf[inters])))
  }

  mm <- mm[mm.rows, ]

  if (!is.null(fun) && fun %in% c("polr", "multinom")) {
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep]
    mm <- mm[, keep]
  }

  pvar <- diag(mm %*% vcm %*% t(mm))


  # condition on random effect variances
  if (type == "re") {
    pvar <- pvar + getVarRand(model)
  }

  se.fit <- sqrt(pvar)

  # shorten to length of fitfram
  if (!is.null(fun) && fun %in% c("polr", "multinom"))
    se.fit <- rep(se.fit, each = dplyr::n_distinct(fitfram$response.level))
  else
    se.fit <- se.fit[1:nrow(fitfram)]

  list(fitfram = fitfram, se.fit = se.fit)
}
