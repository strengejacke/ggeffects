# get standard errors of predictions from model matrix and vcov ----

.standard_error_predictions <- function(
  model,
  prediction_data,
  value_adjustment,
  terms,
  model_class = NULL,
  type = "fe",
  vcov.fun = NULL,
  vcov.type = NULL,
  vcov.args = NULL,
  condition = NULL,
  interval = NULL) {

  se <- tryCatch(
    {
      .safe_se_from_vcov(
        model,
        prediction_data,
        value_adjustment,
        terms,
        model_class,
        type,
        vcov.fun,
        vcov.type,
        vcov.args,
        condition,
        interval
      )
    },
    error = function(x) { x },
    warning = function(x) { NULL },
    finally = function(x) { NULL }
  )

  if (is.null(se) || inherits(se, c("error", "simpleError"))) {
    insight::print_color("Error: Confidence intervals could not be computed.\n", "red")
    if (inherits(se, c("error", "simpleError"))) {
      cat(sprintf("* Reason: %s\n", .safe_deparse(se[[1]])))
      err.source <- .safe_deparse(se[[2]])
      if (all(grepl("^(?!(safe_se_from_vcov))", err.source, perl = TRUE))) {
        cat(sprintf("* Source: %s\n", err.source))
      }
    }
    se <- NULL
  }

  se
}

#' @importFrom stats model.matrix terms formula
#' @importFrom purrr flatten_chr map2
#' @importFrom insight find_random clean_names find_parameters get_varcov find_formula
.safe_se_from_vcov <- function(model,
                              prediction_data,
                              value_adjustment,
                              terms,
                              model_class,
                              type,
                              vcov.fun,
                              vcov.type,
                              vcov.args,
                              condition,
                              interval) {

  model_frame <- insight::get_data(model)

  # check random effect terms. We can't compute SE if data has
  # factors with only one level, however, if user conditions on
  # random effects and only conditions on one level, it is indeed
  # possible to calculate SE - so, ignore random effects for the
  # check of one-level-factors only

  re.terms <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)


  # we can't condition on categorical variables

  if (!is.null(condition)) {
    cn <- names(condition)
    cn.factors <- sapply(cn, function(.x) is.factor(model_frame[[.x]]) && !(.x %in% re.terms))
    condition <- condition[!cn.factors]
    if (.is_empty(condition)) condition <- NULL
  }


  # copy data frame with predictions
  newdata <- .data_grid(
    model,
    model_frame,
    terms,
    value_adjustment = value_adjustment,
    factor_adjustment = FALSE,
    show_pretty_message = FALSE,
    condition = condition
  )

  # make sure we have enough values to compute CI
  nlevels_terms <- sapply(
    colnames(newdata),
    function(.x) !(.x %in% re.terms) && is.factor(newdata[[.x]]) && nlevels(newdata[[.x]]) == 1
  )

  if (any(nlevels_terms)) {
    not_enough <- colnames(newdata)[which(nlevels_terms)[1]]
    remove_lvl <- paste0("[", gsub(pattern = "(.*)\\[(.*)\\]", replacement = "\\2", x = terms[which(.clean_terms(terms) == not_enough)]), "]", collapse = "")
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
  newdata <- cbind(as.list(new.resp), newdata)

  # clean terms from brackets
  terms <- .clean_terms(terms)

  # sort data by grouping levels, so we have the correct order
  # to slice data afterwards
  if (length(terms) > 2) {
    trms <- terms[3]
    newdata <- newdata[order(newdata[[trms]]), ]
    prediction_data <- prediction_data[order(prediction_data[[trms]]), ]
  }

  if (length(terms) > 1) {
    trms <- terms[2]
    newdata <- newdata[order(newdata[[trms]]), ]
    prediction_data <- prediction_data[order(prediction_data[[trms]]), ]
  }

  trms <- terms[1]
  newdata <- newdata[order(newdata[[trms]]), ]
  prediction_data <- prediction_data[order(prediction_data[[trms]]), ]

  # rownames were resorted as well, which causes troubles in model.matrix
  rownames(newdata) <- NULL
  rownames(prediction_data) <- NULL

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


  model_terms <- tryCatch({
    stats::terms(model)
  },
  error = function(e) {
    insight::find_formula(model)$conditional
  })

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

    add.terms <- purrr::map2(contrs, names(contrs), function(.x, .y) {
      f <- model_frame[[.y]]
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
  if (.is_empty(mm.rows) || !all(terms %in% colnames(mmdf))) {
    inters <- which(insight::clean_names(colnames(mmdf)) %in% terms)
    mm.rows <- as.numeric(rownames(unique(mmdf[inters])))
  }

  mm <- mm[mm.rows, ]

  if (!is.null(model_class) && model_class %in% c("polr", "mixor", "multinom", "brmultinom", "bracl", "fixest")) {
    keep <- intersect(colnames(mm), colnames(vcm))
    vcm <- vcm[keep, keep]
    mm <- mm[, keep]
  }

  pvar <- diag(mm %*% vcm %*% t(mm))
  pr_int <- FALSE

  # condition on random effect variances
  if (type == "re" || (!is.null(interval) && interval == "prediction")) {
    sig <- .get_random_effect_variance(model)
    if (sig > 0.0001) {
      pvar <- pvar + sig
      pr_int <- TRUE
    }
  }

  se.fit <- sqrt(pvar)

  # shorten to length of prediction_data
  if (!is.null(model_class) && model_class %in% c("polr", "multinom", "mixor"))
    se.fit <- rep(se.fit, each = .n_distinct(prediction_data$response.level))
  else
    se.fit <- se.fit[1:nrow(prediction_data)]

  std_error <- list(prediction_data = prediction_data, se.fit = se.fit)
  attr(std_error, "prediction_interval") <- pr_int

  std_error
}
