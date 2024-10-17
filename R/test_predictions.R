#' @title (Pairwise) comparisons between predictions (marginal effects)
#' @name test_predictions
#'
#' @description Function to test differences of adjusted predictions for
#'   statistical significance. This is usually called contrasts or (pairwise)
#'   comparisons, or "marginal effects". `hypothesis_test()` is an alias.
#'
#' @param object A fitted model object, or an object of class `ggeffects`. If
#' `object` is of class `ggeffects`, arguments `terms`, `margin` and `ci_level`
#' are taken from the `ggeffects` object and don't need to be specified.
#' @param test Hypothesis to test, defined as character string. Can be one of:
#'
#'   - `"pairwise"` (default), to test pairwise comparisons.
#'   - `"trend"` (or `"slope"`) to test for the linear trend/slope of (usually)
#'     continuous predictors. These options are just aliases for setting
#'     `trend = NULL`.
#'   - `"contrast"` to test simple contrasts (i.e. each level is tested against
#'     the average over _all_ levels).
#'   - `"exclude"` to test simple contrasts (i.e. each level is tested against
#'     the average over _all other_ levels, excluding the contrast that is being
#'     tested).
#'   - `"interaction"` to test interaction contrasts (difference-in-difference
#'     contrasts). More flexible interaction contrasts can be calcualted using
#'     the `test_args` argument.
#'   - `"consecutive"` to test contrasts between consecutive levels of a predictor.
#'   - `"polynomial"` to test orthogonal polynomial contrasts, assuming
#'     equally-spaced factor levels.
#'   - A character string with a custom hypothesis, e.g. `"b2 = b1"`. This would
#'     test if the second level of a predictor is different from the first level.
#'     Custom hypotheses are very flexible. It is also possible to test interaction
#'     contrasts (difference-in-difference contrasts) with custom hypotheses, e.g.
#'     `"(b2 - b1) = (b4 - b3)"`. See also section _Introduction into contrasts
#'     and pairwise comparisons_.
#'   - A data frame with custom contrasts. See 'Examples'.
#'   - `NULL`, in which case simple contrasts are computed.
#'
#' Technical details about the packages used as back-end to calculate contrasts
#' and pairwise comparisons are provided in the section _Packages used as back-end
#' to calculate contrasts and pairwise comparisons_ below.
#' @param test_args Optional arguments passed to `test`, typically provided as
#' named list. Only applies to those options that use the **emmeans** package
#' as backend, e.g. if `test = "interaction"`, `test_args` will be passed to
#' `emmeans::contrast(interaction = test_args)`. For other *emmeans* options
#' (like `"cotrast"`, `"exclude"`, `"consecutive"` and so on), `test_args` will
#' be passed to the `option` argument in `emmeans::contrast()`.
#' @param terms If `object` is an object of class `ggeffects`, the same `terms`
#' argument is used as for the predictions, i.e. `terms` can be ignored. Else,
#' if `object` is a model object, `terms` must be a character vector with the
#' names of the focal terms from `object`, for which contrasts or comparisons
#' should be displayed. At least one term is required, maximum length is three
#' terms. If the first focal term is numeric, contrasts or comparisons for the
#' *slopes* of this numeric predictor are computed (possibly grouped by the
#' levels of further categorical focal predictors).
#' @param by Character vector specifying the names of predictors to condition on.
#' Hypothesis test is then carried out for focal terms by each level of `by`
#' variables. This is useful especially for interaction terms, where we want
#' to test the interaction within "groups". `by` is only relevant for
#' categorical predictors.
#' @param margin Character string, indicates the method how to marginalize over
#' non-focal terms. See [`predict_response()`] for details. If `object` is an
#' object of class `ggeffects`, the same `margin` argument is used as for the
#' predictions, i.e. `margin` can be ignored.
#' @param scale Character string, indicating the scale on which the contrasts
#' or comparisons are represented. Can be one of:
#'
#' - `"response"` (default), which would return contrasts on the response
#'   scale (e.g. for logistic regression, as probabilities);
#' - `"link"` to return contrasts on scale of the linear predictors
#'   (e.g. for logistic regression, as log-odds);
#' - `"probability"` (or `"probs"`) returns contrasts on the probability scale,
#'   which is required for some model classes, like `MASS::polr()`;
#' - `"oddsratios"` to return contrasts on the odds ratio scale (only applies
#'   to logistic regression models);
#' - `"irr"` to return contrasts on the odds ratio scale (only applies to
#'   count models);
#' - or a transformation function like `"exp"` or `"log"`, to return transformed
#'   (exponentiated respectively logarithmic) contrasts; note that these
#'   transformations are applied to the _response scale_.
#'
#' **Note:** If the `scale` argument is not supported by the provided `object`,
#' it is automatically changed to a supported scale-type (a message is printed
#' when `verbose = TRUE`).
#' @param equivalence ROPE's lower and higher bounds. Should be `"default"` or
#' a vector of length two (e.g., `c(-0.1, 0.1)`). If `"default"`,
#' [`bayestestR::rope_range()`] is used. Instead of using the `equivalence`
#' argument, it is also possible to call the `equivalence_test()` method
#' directly. This requires the **parameters** package to be loaded. When
#' using `equivalence_test()`, two more columns with information about the
#' ROPE coverage and decision on H0 are added. Furthermore, it is possible
#' to `plot()` the results from `equivalence_test()`. See
#' [`bayestestR::equivalence_test()`] resp. [`parameters::equivalence_test.lm()`]
#' for details.
#' @param p_adjust Character vector, if not `NULL`, indicates the method to
#' adjust p-values. See [`stats::p.adjust()`] or [`stats::p.adjust.methods`]
#' for details. Further possible adjustment methods are `"tukey"` or `"sidak"`,
#' and for `johnson_neyman()`, `"fdr"` (or `"bh"`) and `"esarey"` (or its
#' short-cut `"es"`) are available options. Some caution is necessary when
#' adjusting p-value for multiple comparisons. See also section _P-value adjustment_
#' below.
#' @param df Degrees of freedom that will be used to compute the p-values and
#' confidence intervals. If `NULL`, degrees of freedom will be extracted from
#' the model using [`insight::get_df()`] with `type = "wald"`.
#' @param ci_level Numeric, the level of the confidence intervals. If `object`
#' is an object of class `ggeffects`, the same `ci_level` argument is used as
#' for the predictions, i.e. `ci_level` can be ignored.
#' @param collapse_levels Logical, if `TRUE`, term labels that refer to identical
#' levels are no longer separated by "-", but instead collapsed into a unique
#' term label (e.g., `"level a-level a"` becomes `"level a"`). See 'Examples'.
#' @param engine Character string, indicates the package to use for computing
#' contrasts and comparisons. Usually, this argument can be ignored, unless you
#' want to explicitly use another package than *marginaleffects* to calculate
#' contrasts and pairwise comparisons. `engine` can be either `"marginaleffects"`
#' (default) or `"emmeans"`. The latter is useful when the **marginaleffects**
#' package is not available, or when the **emmeans** package is preferred. Note
#' that using **emmeans** as back-end is currently not as feature rich as the default
#' (**marginaleffects**) and still in development. Setting `engine = "emmeans"`
#' provides some additional test options: `"interaction"` to calculate interaction
#' contrasts, `"consecutive"` to calculate contrasts between consecutive levels of a
#' predictor, or a data frame with custom contrasts (see also `test`). There is
#' an experimental option as well, `engine = "ggeffects"`. However, this is
#' currently work-in-progress and offers much less options as the default engine,
#' `"marginaleffects"`. It can be faster in some cases, though, and works for
#' comparing predicted random effects in mixed models, or predicted probabilities
#' of the zero-inflation component. If the **marginaleffects** package is not
#' installed, the **emmeans** package is used automatically. If this package is
#' not installed as well, `engine = "ggeffects"` is used.
#' @param verbose Toggle messages and warnings.
#' @param ... Arguments passed down to [`data_grid()`] when creating the reference
#' grid and to [`marginaleffects::predictions()`] resp. [`marginaleffects::slopes()`].
#' For instance, arguments `type` or `transform` can be used to back-transform
#' comparisons and contrasts to different scales. `vcov` can be used to
#' calculate heteroscedasticity-consistent standard errors for contrasts.
#' See examples at the bottom of
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_comparisons_1.html)
#' for further details.
#'
#' To define a heteroscedasticity-consistent variance-covariance matrix, you can
#' either use the same arguments as for `predict_response()` etc., namely `vcov`
#' and `vcov_args`. These are then transformed into a matrix and passed down to
#' the `vcov` argument in **marginaleffects**. Or you directly use the `vcov`
#' argument. See `?marginaleffects::slopes` for further details.
#'
#' @seealso There is also an `equivalence_test()` method in the **parameters**
#' package ([`parameters::equivalence_test.lm()`]), which can be used to
#' test contrasts or comparisons for practical equivalence. This method also
#' has a `plot()` method, hence it is possible to do something like:
#' ```
#' library(parameters)
#' predict_response(model, focal_terms) |>
#'   equivalence_test() |>
#'   plot()
#' ```
#'
#' @section Introduction into contrasts and pairwise comparisons:
#'
#' There are many ways to test contrasts or pairwise comparisons. A
#' detailed introduction with many (visual) examples is shown in
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_comparisons_1.html).
#'
#' @section Simple workflow for pairwise comparisons:
#'
#' A simple workflow includes calculating adjusted predictions and passing the
#' results directly to `test_predictions()`, e.g.:
#' ```
#' # 1. fit your model
#' model <- lm(mpg ~ hp + wt + am, data = mtcars)
#' # 2. calculate adjusted predictions
#' pr <- predict_response(model, "am")
#' pr
#' # 3. test pairwise comparisons
#' test_predictions(pr)
#' ```
#' See also [this vignette](https://strengejacke.github.io/ggeffects/articles/practical_glm_workflow.html).
#'
#' @section Packages used as back-end to calculate contrasts and pairwise comparisons:
#'
#' The `test` argument is used to define which kind of contrast or comparison
#' should be calculated. The default is to use the **marginaleffects** package.
#' Here are some technical details about the packages used as back-end. When
#' `test` is...
#'   - `"pairwise"` (default), pairwise comparisons are based on the **marginaleffects**
#'     package.
#'   - `"trend"` or `"slope"` also uses the **marginaleffects** package.
#'   - `"contrast"` uses the **emmeans** package, i.e. `emmeans::contrast(method = "eff")`
#'     is called.
#'   - `"exclude"` relies on the **emmeans** package, i.e. `emmeans::contrast(method = "del.eff")`
#'     is called.
#'   - `"polynomial"` relies on the **emmeans** package, i.e. `emmeans::contrast(method = "poly")`
#'     is called.
#'   - `"interaction"` uses the **emmeans** package, i.e. `emmeans::contrast(interaction = ...)`
#'     is called.
#'   - `"consecutive"` also relies on the **emmeans** package, i.e.
#'     `emmeans::contrast(method = "consec")` is called.
#'   - a character string with a custom hypothesis, the **marginaleffects**
#'     package is used.
#'   - a data frame with custom contrasts, **emmeans** is used again.
#'   - `NULL` calls functions from the **marginaleffects** package with
#'     `hypothesis = NULL`.
#'   - If all focal terms are only present as random effects in a mixed model,
#'     or if predicted probabilities for the zero-inflation component of a model
#'     should be tested, functions from the **ggeffects** package are used. There
#'     is an example for pairwise comparisons of random effects in
#'     [this vignette](https://strengejacke.github.io/ggeffects/articles/practical_intersectionality.html).
#'
#' @section P-value adjustment for multiple comparisons:
#'
#' Note that p-value adjustment for methods supported by `p.adjust()` (see also
#' `p.adjust.methods`), each row is considered as one set of comparisons, no
#' matter which `test` was specified. That is, for instance, when `test_predictions()`
#' returns eight rows of predictions (when `test = NULL`), and `p_adjust = "bonferroni"`,
#' the p-values are adjusted in the same way as if we had a test of pairwise
#' comparisons (`test = "pairwise"`) where eight rows of comparisons are
#' returned. For methods `"tukey"` or `"sidak"`, a rank adjustment is done
#' based on the number of combinations of levels from the focal predictors
#' in `terms`. Thus, the latter two methods may be useful for certain tests
#' only, in particular pairwise comparisons.
#'
#' For `johnson_neyman()`, the only available adjustment methods are `"fdr"`
#' (or `"bh"`) (_Benjamini & Hochberg (1995)_) and `"esarey"` (or `"es"`)
#' (_Esarey and Sumner 2017_). These usually return similar results. The major
#' difference is that `"fdr"` can be slightly faster and more stable in edge
#' cases, however, confidence intervals are not updated. Only the p-values are
#' adjusted. `"esarey"` is slower, but confidence intervals are updated as well.
#'
#' @inheritSection print Global Options to Customize Tables when Printing
#'
#' @section Global options to choose package for calculating comparisons:
#'
#' `ggeffects_test_engine` can be used as option to either use the **marginaleffects**
#' package for computing contrasts and comparisons (default), or the **emmeans**
#' package (e.g. `options(ggeffects_test_engine = "emmeans")`). The latter is
#' useful when the **marginaleffects** package is not available, or when the
#' **emmeans** package is preferred. You can also provide the engine directly, e.g.
#' `test_predictions(..., engine = "emmeans")`. Note that using **emmeans** as
#' backend is currently not as feature rich as the default (**marginaleffects**)
#' and still in development.
#'
#' If `engine = "emmeans"`, the `test` argument can also be `"interaction"`
#' to calculate interaction contrasts (difference-in-difference contrasts),
#' `"consecutive"` to calculate contrasts between consecutive levels of a predictor,
#' or a data frame with custom contrasts. If `test` is one of the latter options,
#' and `engine` is not specified, the `engine` is automatically set to `"emmeans"`.
#' Additionally, the `test_args` argument can be used to specify further options
#' for those contrasts. See 'Examples' and documentation of `test_args`.
#'
#' If the **marginaleffects** package is not installed, the **emmeans** package is
#' used automatically. If this package is not installed as well,
#' `engine = "ggeffects"` is used.
#'
#' @return A data frame containing predictions (e.g. for `test = NULL`),
#' contrasts or pairwise comparisons of adjusted predictions or estimated
#' marginal means.
#'
#' @references
#' Esarey, J., & Sumner, J. L. (2017). Marginal effects in interaction models:
#' Determining and controlling the false positive rate. Comparative Political
#' Studies, 1–33. Advance online publication. doi: 10.1177/0010414017730080
#'
#' @examplesIf requireNamespace("marginaleffects") && requireNamespace("parameters") && interactive()
#' \donttest{
#' data(efc)
#' efc$c172code <- as.factor(efc$c172code)
#' efc$c161sex <- as.factor(efc$c161sex)
#' levels(efc$c161sex) <- c("male", "female")
#' m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
#'
#' # direct computation of comparisons
#' test_predictions(m, "c172code")
#'
#' # passing a `ggeffects` object
#' pred <- predict_response(m, "c172code")
#' test_predictions(pred)
#'
#' # test for slope
#' test_predictions(m, "c12hour")
#'
#' # interaction - contrasts by groups
#' m <- lm(barthtot ~ c12hour + c161sex * c172code + neg_c_7, data = efc)
#' test_predictions(m, c("c161sex", "c172code"), test = NULL)
#'
#' # interaction - pairwise comparisons by groups
#' test_predictions(m, c("c161sex", "c172code"))
#'
#' # equivalence testing
#' test_predictions(m, c("c161sex", "c172code"), equivalence = c(-2.96, 2.96))
#'
#' # equivalence testing, using the parameters package
#' pr <- predict_response(m, c("c161sex", "c172code"))
#' parameters::equivalence_test(pr)
#'
#' # interaction - collapse unique levels
#' test_predictions(m, c("c161sex", "c172code"), collapse_levels = TRUE)
#'
#' # p-value adjustment
#' test_predictions(m, c("c161sex", "c172code"), p_adjust = "tukey")
#'
#' # not all comparisons, only by specific group levels
#' test_predictions(m, "c172code", by = "c161sex")
#'
#' # specific comparisons
#' test_predictions(m, c("c161sex", "c172code"), test = "b2 = b1")
#'
#' # interaction - slope by groups
#' m <- lm(barthtot ~ c12hour + neg_c_7 * c172code + c161sex, data = efc)
#' test_predictions(m, c("neg_c_7", "c172code"))
#'
#' # Interaction and consecutive contrasts -----------------
#' # -------------------------------------------------------
#' data(coffee_data, package = "ggeffects")
#' m <- lm(alertness ~ time * coffee + sex, data = coffee_data)
#'
#' # consecutive contrasts
#' test_predictions(m, "time", by = "coffee", test = "consecutive")
#'
#' # interaction contrasts - difference-in-difference comparisons
#' pr <- predict_response(m, c("time", "coffee"), margin = "marginalmeans")
#' test_predictions(pr, test = "interaction")
#'
#' # Custom contrasts --------------------------------------
#' # -------------------------------------------------------
#' wakeup_time <- data.frame(
#'   "wakeup vs later" = c(-2, 1, 1) / 2, # make sure each "side" sums to (+/-)1!
#'   "start vs end of day" = c(-1, 0, 1)
#' )
#' test_predictions(m, "time", by = "coffee", test = wakeup_time)
#'
#' # Example: marginal effects -----------------------------
#' # -------------------------------------------------------
#' data(iris)
#' m <- lm(Petal.Width ~ Petal.Length + Species, data = iris)
#'
#' # we now want the marginal effects for "Species". We can calculate
#' # the marginal effect using the "marginaleffects" package
#' marginaleffects::avg_slopes(m, variables = "Species")
#'
#' # finally, test_predictions() returns the same. while the previous results
#' # report the marginal effect compared to the reference level "setosa",
#' # test_predictions() returns the marginal effects for all pairwise comparisons
#' test_predictions(m, "Species")
#' }
#' @export
test_predictions <- function(object, ...) {
  UseMethod("test_predictions")
}


#' @rdname test_predictions
#' @export
hypothesis_test <- test_predictions


#' @rdname test_predictions
#' @export
test_predictions.default <- function(object,
                                     terms = NULL,
                                     by = NULL,
                                     test = "pairwise",
                                     test_args = NULL,
                                     equivalence = NULL,
                                     scale = "response",
                                     p_adjust = NULL,
                                     df = NULL,
                                     ci_level = 0.95,
                                     collapse_levels = FALSE,
                                     margin = "mean_reference",
                                     engine = "marginaleffects",
                                     verbose = TRUE,
                                     ...) {
  # margin-argument -----------------------------------------------------------
  # harmonize the "margin" argument
  # ---------------------------------------------------------------------------

  # default for "margin" argument?
  margin <- getOption("ggeffects_margin", margin)
  # validate "margin" argument
  margin <- .check_arg(
    margin,
    c(
      "mean_reference", "mean_mode", "marginalmeans", "empirical",
      "counterfactual", "full_data", "ame", "marginaleffects"
    )
  )
  # harmonize argument
  margin <- switch(margin,
    mean_reference = ,
    mean_mode = "mean_reference",
    marginalmeans = "marginalmeans",
    "empirical"
  )

  # handle alias - "slope" or "trend" are aliases for simply setting it to NULL
  if (!is.null(test) && !is.data.frame(test) && test %in% c("trend", "slope")) {
    test <- NULL
  }

  # check for installed packages ----------------------------------------------
  # check if we have the appropriate package version installed
  # ---------------------------------------------------------------------------

  # default for "engine" argument?
  engine <- getOption("ggeffects_test_engine", engine)
  # validate "engine" argument
  engine <- .safe(match.arg(engine, c("marginaleffects", "emmeans")))

  # throw error if invalid engine
  if (is.null(engine)) {
    insight::format_error(
      "Argument `engine` must be either \"marginaleffects\" or \"emmeans\". If you want to use `engine = \"ggeffects\"`, you need to provide the `ggeffects` object directly, e.g.:", # nolint
      paste0("\n  ", insight::color_text("pr <- predict_response(model, ...)", "cyan")),
      insight::color_text("test_predictions(pr, engine = \"ggeffects\")", "cyan")
    )
  }

  # for test = "interaction" or "consecutive", we need to call emmeans!
  if (.is_emmeans_contrast(test)) {
    engine <- "emmeans"
  }
  if (!insight::check_if_installed("marginaleffects", quietly = TRUE) && engine == "marginaleffects") { # nolint
    engine <- "emmeans"
  }
  # if we don't have the package installed, we switch to emmeans
  if (!insight::check_if_installed("emmeans", quietly = TRUE) && engine == "emmeans") {
    # if we even don't have emmeans, we throw an error
    insight::format_error(
      "The `marginaleffects` and `emmeans` packages are required for this function. Please install them from CRAN by running `install.packages(c(\"emmeans\", \"marginaleffects\"))`." # nolint
    )
  }

  # when model is a "ggeffects" object, due to environment issues, "model"
  # can be NULL (in particular in tests), thus check for NULL
  if (is.null(object)) {
    insight::format_error(
      "No model object found. Try to directly pass the model object to `test_predictions()`, e.g.:",
      paste0("\n  ", insight::color_text("model <- lm(mpg ~ gear, data = mtcars)", "cyan")),
      insight::color_text("test_predictions(model, \"gear\")", "cyan")
    )
  }

  # for gamm/gamm4 objects, we have a list with two items, mer and gam
  # extract just the gam-part then
  if (is.gamm(object) || is.gamm4(object)) {
    object <- object$gam
  }

  # only model objects are supported...
  if (!insight::is_model_supported(object)) {
    insight::format_error(
      paste0("Objects of class `", class(object)[1], "` are not yet supported.")
    )
  }

  # for parsnip-models, use $fit element
  if (inherits(object, "model_fit")) {
    object <- object$fit
  }

  # ci-level cannot be NA, set to default value then
  if (is.na(ci_level)) {
    ci_level <- 0.95
  }
  miss_scale <- missing(scale)

  # random effects -----------------------------------------------------------
  # check if we have random effects in the model, and if pairwise comparisons
  # should be done for randome effects only (i.e. all focal terms are specified
  # as random effects in the model). If so, we need to tell the user that they
  # have to use `engine = "ggeffects"`
  # ---------------------------------------------------------------------------

  random_pars <- insight::find_random(object, split_nested = TRUE, flatten = TRUE)
  if (verbose && !is.null(random_pars) && all(.clean_terms(terms) %in% random_pars)) {
    insight::format_alert(
      "All focal terms are included as random effects in the model. To calculate pairwise comparisons for random effects, use `engine = \"ggeffects\"`." # nolint
    )
  }


  # dot-arguments -------------------------------------------------------------
  # evaluate dots - remove conflicting additional arguments. We have our own
  # "scale" argument that modulates the "type" and "transform" arguments
  # in "marginaleffects"
  # ---------------------------------------------------------------------------

  dot_args <- list(...)
  # default scale is response scale without any transformation
  dot_args$transform <- NULL
  dot_args$type <- "response"
  if (scale == "link") {
    dot_args$type <- "link"
  } else if (scale %in% c("probability", "probs")) {
    dot_args$type <- "probs"
  } else if (scale == "exp") {
    dot_args$transform <- "exp"
  } else if (scale == "log") {
    dot_args$transform <- "ln"
  } else if (scale %in% c("irr", "oddsratios")) {
    dot_args$type <- "link"
    dot_args$transform <- "exp"
  } else {
    # unknown type - might be supported by marginal effects
    dot_args$type <- scale
  }

  # make sure we have a valid type-argument...
  dot_args$type <- .sanitize_type_argument(object, dot_args$type, verbose = ifelse(miss_scale, FALSE, verbose))

  # make sure we have a valid vcov-argument when user supplies "standard" vcov-arguments
  # from ggpredict, like "vcov" etc. - then remove vcov-arguments
  if (!is.null(dot_args$vcov)) {
    dot_args$vcov <- .get_variance_covariance_matrix(object, dot_args$vcov, dot_args$vcov_args)
    # remove non supported args
    dot_args$vcov_args <- NULL
  }

  # new policy for glmmTMB models
  if (inherits(object, "glmmTMB") && is.null(dot_args$vcov)) {
    dot_args$vcov <- TRUE
  }

  # engine --------------------------------------------------------------------
  # here we switch to emmeans, if "engine" is set to "emmeans"
  # ---------------------------------------------------------------------------
  if (engine == "emmeans") {
    return(.test_predictions_emmeans(
      object,
      terms = terms,
      by = by,
      test = test,
      test_args = test_args,
      equivalence = equivalence,
      scale = scale,
      p_adjust = p_adjust,
      df = df,
      ci_level = ci_level,
      collapse_levels = collapse_levels,
      margin = margin,
      verbose = verbose,
      ...
    ))
  }

  minfo <- insight::model_info(object, verbose = FALSE)
  model_data <- .get_model_data(object)

  # make sure that we have logistic regression when scale is "oddsratios"
  if (scale == "oddsratios" && !minfo$is_logit) {
    insight::format_error(
      "Argument `scale = \"oddsratios\"` is only supported for logistic regression models."
    )
  }
  # make sure that we have count regression when scale is "irr"
  if (scale == "irr" && !minfo$is_count) {
    insight::format_error(
      "Argument `scale = \"irr\"` is only supported for count models (Poisson, negative binomial, ...)."
    )
  }

  # for mixed models, we need different handling later...
  need_average_predictions <- include_random <- insight::is_mixed_model(object)
  msg_intervals <- FALSE
  # for "marginalmeans", don't condition on random effects
  if (margin == "marginalmeans" && include_random) {
    include_random <- FALSE
    dot_args$re.form <- NA
  }

  # for zero-inflation probabiliries, we need to set "need_average_predictions"
  # to FALSE, else no confidence intervals will be returned.
  if (scale %in% c("zi_prob", "zero", "zprob")) {
    need_average_predictions <- FALSE
  }

  # by-variables are included in terms
  if (!is.null(by)) {
    terms <- unique(c(terms, by))
  }

  # we want contrasts or comparisons for these focal predictors...
  focal <- .clean_terms(terms)
  random_group <- NULL

  # check if we have a mixed model - in this case, we need to ensure that our
  # random effect variable (group factor) is included in the grid
  if (need_average_predictions) {
    random_group <- insight::find_random(object, split_nested = TRUE, flatten = TRUE)
    if (!all(random_group %in% terms)) {
      terms <- unique(c(terms, random_group))
    }
    # tell user that intervals for comparisons are confidence, not prediction intervals
    if (verbose && any(random_group %in% focal)) {
      msg_intervals <- TRUE
    }
  }

  # data grids -----------------------------------------------------------------
  # we need data grids only for marginal means or mean/mode, not for
  # counterfactuals predictions that average over non-focal terms.
  # ----------------------------------------------------------------------------

  # data grid, varies depending on "margin" argument. For "marginalmeans",
  # we need a balanced data grid
  if (margin == "marginalmeans") {
    datagrid <- marginaleffects::datagrid(model = object, grid_type = "balanced")
  } else {
    datagrid <- data_grid(object, terms, group_factor = random_group, ...)
    # make sure characters are preserved
    for (i in colnames(datagrid)) {
      if (i %in% colnames(model_data) && is.character(model_data[[i]])) {
        datagrid[[i]] <- as.character(datagrid[[i]])
      }
    }
  }

  # check for valid by-variable
  by <- .validate_by_argument(by, datagrid)
  by_arg <- NULL
  # for models with ordinal/categorical outcome, we need focal terms and
  # response variable as "by" argument
  if (minfo$is_ordinal || minfo$is_multinomial) {
    by_arg <- unique(c(focal, insight::find_response(object)))
  } else if (margin %in% c("marginalmeans", "empirical")) {
    # for "marginalmeans", when we have a balanced data grid, we also need the
    # focal term(s) as by-argument. And we need them for "empirical".
    by_arg <- focal
  }

  # sanity check - variable names in the grid should not "mask" standard names
  # from the marginal effects output
  reserved <- c(
    "group", "term", "contrast", "estimate", "std.error", "statistic", "conf.low",
    "conf.high", "p.value", "p.value.nonsup", "p.value.noninf", "type"
  )
  invalid_names <- reserved %in% colnames(datagrid)
  if (any(invalid_names)) {
    insight::format_error(
      "Some variable names in the model are not allowed when using `test_predictions()` because they are reserved by the internally used {.pkg marginaleffects} package.", # nolint
      paste0("Please rename following variables and fit your model again: ", toString(paste0("`", reserved[invalid_names], "`"))) # nolint
    )
  }

  # comparisons only make sense if we have at least two predictors, or if
  # we have one categorical
  focal_numeric <- vapply(datagrid[focal], is.numeric, TRUE)
  focal_other <- !focal_numeric
  hypothesis_label <- rope_range <- NULL

  # do we want an equivalence-test?
  if (!is.null(equivalence)) {
    if (all(equivalence == "default")) {
      insight::check_if_installed("bayestestR")
      rope_range <- bayestestR::rope_range(object)
    } else {
      rope_range <- equivalence
    }
  }

  # extract degrees of freedom
  if (is.null(df)) {
    df <- .get_df(object)
  }

  # ===========================================================================
  # the following, very long code block, mainly does two things: first, extract
  # the requested pairwise comparisons or contrasts, either for slopes or for
  # categorical predictors. The result is a data frame named ".comparisons".
  # second, a very long block of code extracts the labels for the contrasts or
  # comparisons, to have nice, readable labels in the printed output. That data
  # frame is named "out". At the end of this function, we combine both data.
  # ===========================================================================

  # numeric focal terms (slopes) ----------------------------------------------
  # we *only* calculate (average) slopes when numeric focal terms come first
  # thus, we don't need to care about the "margin" argument here
  # ---------------------------------------------------------------------------

  # if *first* focal predictor is numeric, compute average slopes
  if (isTRUE(focal_numeric[1])) {
    # just the "trend" (slope) of one focal predictor
    if (length(focal) == 1) {
      # contrasts of slopes ---------------------------------------------------
      # here comes the code to test wether a slope is significantly different
      # from null (contrasts)
      # -----------------------------------------------------------------------

      # argument "test" will be ignored for average slopes
      test <- NULL
      # prepare argument list for "marginaleffects::slopes"
      # we add dot-args later, that modulate the scale of the contrasts
      fun_args <- list(
        model = object,
        variables = focal,
        df = df,
        conf_level = ci_level
      )
      .comparisons <- .call_me("avg_slopes", fun_args, dot_args, include_random)
      # "extracting" labels for this simple case is easy...
      out <- data.frame(x_ = "slope", stringsAsFactors = FALSE)
      colnames(out) <- focal
    } else {
      # pairwise comparison of slopes -----------------------------------------
      # here comes the code to test wether slopes between groups are
      # significantly different from each other (pairwise comparison)
      # -----------------------------------------------------------------------

      # prepare argument list for "marginaleffects::slopes"
      # we add dot-args later, that modulate the scale of the contrasts
      fun_args <- list(
        model = object,
        variables = focal[1],
        by = focal[2:length(focal)],
        newdata = datagrid,
        hypothesis = test,
        df = df,
        conf_level = ci_level
      )
      # "trends" (slopes) of numeric focal predictor by group levels
      # of other focal predictor
      .comparisons <- .call_me("slopes", fun_args, dot_args, include_random)

      # labelling terms ------------------------------------------------------
      # here comes the code for extracting nice term labels
      # ----------------------------------------------------------------------

      # for pairwise comparisons, we need to extract contrasts
      if (!is.null(test) && all(test == "pairwise")) {
        # pairwise comparisons of slopes --------------------------------------
        # here comes the code to extract labels for pairwise comparison of slopes
        # ---------------------------------------------------------------------

        # before we extract labels, we need to check whether any factor level
        # contains a "," - in this case, strplit() will not work properly
        .comparisons$term <- .fix_comma_levels(.comparisons$term, datagrid, focal)

        # if we find a comma in the terms column, we have two categorical predictors
        if (any(grepl(",", .comparisons$term, fixed = TRUE))) {
          contrast_terms <- data.frame(
            do.call(rbind, strsplit(.comparisons$term, " - ", fixed = TRUE)),
            stringsAsFactors = FALSE
          )

          # split and recombine term names
          pairs1 <- unlist(strsplit(contrast_terms[[1]], ",", fixed = TRUE))
          pairs2 <- unlist(strsplit(contrast_terms[[2]], ",", fixed = TRUE))
          contrast_pairs <- paste0(
            insight::trim_ws(pairs1),
            "-",
            insight::trim_ws(pairs2)
          )

          # create data frame - since we have two categorical predictors at
          # this point (and one numerical), we create a data frame with three
          # columns (one per focal term).
          out <- data.frame(
            x_ = "slope",
            x__ = contrast_pairs[c(TRUE, FALSE)],
            x___ = contrast_pairs[c(FALSE, TRUE)],
            stringsAsFactors = FALSE
          )
        } else {
          out <- data.frame(
            x_ = "slope",
            x__ = gsub(" ", "", .comparisons$term, fixed = TRUE),
            stringsAsFactors = FALSE
          )
        }
        colnames(out) <- focal
      } else if (is.null(test)) {
        # contrasts of slopes -------------------------------------------------
        # here comes the code to extract labels for trends of slopes
        # ---------------------------------------------------------------------

        # if we have simple slopes without pairwise comparisons, we can
        # copy the information directly from the marginaleffects-object
        grid_categorical <- as.data.frame(
          .comparisons[focal[2:length(focal)]],
          stringsAsFactors = FALSE
        )
        out <- cbind(data.frame(x_ = "slope", stringsAsFactors = FALSE), grid_categorical)
        colnames(out) <- focal
      } else {
        # hypothesis testing of slopes ----------------------------------------
        # here comes the code to extract labels for special hypothesis tests
        # ---------------------------------------------------------------------

        # if we have specific comparisons of estimates, like "b1 = b2", we
        # want to replace these shortcuts with the full related predictor names
        # and levels
        if (any(grepl("b[0-9]+", .comparisons$term))) {
          # prepare argument list for "marginaleffects::slopes"
          # we add dot-args later, that modulate the scale of the contrasts
          fun_args <- list(
            model = object,
            variables = focal[1],
            by = focal[2:length(focal)],
            hypothesis = NULL,
            df = df,
            conf_level = ci_level
          )
          # re-compute comoparisons for all combinations, so we know which
          # estimate refers to which combination of predictor levels
          .full_comparisons <- .call_me("slopes", fun_args, dot_args, include_random)
          # replace "hypothesis" labels with names/levels of focal predictors
          hypothesis_label <- .extract_labels(
            full_comparisons = .full_comparisons,
            focal = focal[2:length(focal)],
            test = test,
            old_labels = .comparisons$term
          )
        }
        # we have a specific hypothesis, like "b3 = b4". We just copy that information
        out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)
      }
    }
    estimate_name <- ifelse(is.null(test), "Slope", "Contrast")
  } else {
    # testing groups (factors) ------------------------------------------------
    # Here comes the code for pairwise comparisons of categorical focal terms
    # -------------------------------------------------------------------------

    by_variables <- NULL # for average predictions
    fun <- "predictions"

    # "variables" argument ----------------------------------------------------
    # for mixed models, and for "marginalmeans" and "empirical", we need to
    # provide the "variables" argument to "marginaleffects::predictions".
    # furthermore, for mixed models, we average across random effects and thus
    # use "avg_predictions" instead of "predictions"
    # -------------------------------------------------------------------------

    if (need_average_predictions) {
      # marginaleffects handles single and multiple variables differently here
      if (length(focal) > 1) {
        by_variables <- sapply(focal, function(i) unique(datagrid[[i]]), simplify = FALSE) # nolint
      } else {
        by_variables <- focal
      }
      by_arg <- NULL
      fun <- "avg_predictions"
    } else if (margin %in% c("marginalmeans", "empirical")) {
      # for "marginalmeans", we need "variables" argument. This must be a list
      # with representative values. Else, we cannot calculate comparisons at
      # representative values of the balanced data grid
      by_variables <- .data_grid(
        object,
        model_frame = model_data,
        terms = terms,
        value_adjustment = "mean",
        emmeans_only = TRUE,
        verbose = FALSE
      )
    }
    # prepare argument list for "marginaleffects::predictions"
    # we add dot-args later, that modulate the scale of the contrasts
    fun_args <- list(
      model = object,
      by = by_arg,
      variables = by_variables,
      newdata = datagrid,
      hypothesis = test,
      df = df,
      conf_level = ci_level
    )
    # for counterfactual predictions, we need no data grid
    if (margin == "empirical") {
      fun_args$newdata <- NULL
    }
    .comparisons <- .call_me(fun, fun_args, dot_args, include_random)

    # nice term labels --------------------------------------------------------
    # here comes the code for extracting nice term labels ---------------------
    # -------------------------------------------------------------------------

    # pairwise comparisons - we now extract the group levels from the "term"
    # column and create separate columns for contrats of focal predictors
    if (!is.null(test) && all(test == "pairwise")) {
      ## pairwise comparisons of group levels -----

      # for "predictions()", term name is "Row 1 - Row 2" etc. For
      # "avg_predictions()", we have "level_a1, level_b1 - level_a2, level_b1"
      # etc. we first want to have a data frame, where each column is one
      # combination of levels, so we split at "," and/or "-".

      # before we extract labels, we need to check whether any factor level
      # contains a "," - in this case, strplit() will not work properly
      .comparisons$term <- .fix_comma_levels(.comparisons$term, datagrid, focal)

      # split and recombine levels of focal terms. We now have a data frame
      # where each column represents one factor level of one focal predictor
      contrast_terms <- data.frame(
        do.call(rbind, strsplit(.comparisons$term, "(,| - )")),
        stringsAsFactors = FALSE
      )
      contrast_terms[] <- lapply(contrast_terms, function(i) {
        # remove certain chars
        for (j in c("(", ")", "Row")) {
          i <- gsub(j, "", i, fixed = TRUE)
        }
        insight::trim_ws(i)
      })

      if (need_average_predictions || margin %in% c("marginalmeans", "empirical")) {
        # in .comparisons$term, for mixed models and when "margin" is either
        # "marginalmeans" or "empirical", we have the factor levels as values,
        # where factor levels from different variables are comma-separated. There
        # are edge cases where we have more than one focal term, but for one of
        # those only one value is requested, e.g.: `terms = c("sex", "education [high]")`
        # in this case, .comparisons$term only contains levels of the first focal
        # term, and no comma (no levels of second focal term are comma separated).
        # in such cases, we simply remove those focal terms, which levels are not
        # appearing in .comparisons$term

        # we first need to get the relevant values / factor levels we want to
        # check for. These can be different from the data grid, when representative
        # values are specified in brackets via the "terms" argument.
        if (is.list(by_variables)) {
          values_to_check <- by_variables
        } else {
          values_to_check <- lapply(datagrid[focal], unique)
        }

        # we then check whether representative values of focal terms actually
        # appear in our pairwise comparisons data frame.
        focal_found <- vapply(values_to_check, function(i) {
          any(vapply(as.character(i), function(j) {
            any(grepl(j, unique(as.character(.comparisons$term)), fixed = TRUE))
          }, TRUE))
        }, TRUE)

        # we temporarily update our focal terms, for extracting labels.
        if (all(focal_found)) {
          updated_focal <- focal
        } else {
          updated_focal <- focal[focal_found]
        }

        # for "avg_predictions()", we already have the correct labels of factor
        # levels, we just need to re-arrange, so that each column represents a
        # pairwise combination of factor levels for each factor
        out <- as.data.frame(lapply(seq_along(updated_focal), function(i) {
          tmp <- contrast_terms[seq(i, ncol(contrast_terms), by = length(updated_focal))]
          unlist(lapply(seq_len(nrow(tmp)), function(j) {
            .contrasts <- as.character(unlist(tmp[j, ]))
            paste(.contrasts, collapse = "-")
          }))
        }), stringsAsFactors = FALSE)
      } else {
        # only for temporary use, for colnames, see below
        updated_focal <- focal
        # check whether we have row numbers, or (e.g., for polr or ordinal models)
        # factor levels. When we have row numbers, we coerce them to numeric and
        # extract related factor levels. Else, in case of ordinal outcomes, we
        # should already have factor levels...
        if (all(vapply(contrast_terms, function(i) anyNA(suppressWarnings(as.numeric(i))), TRUE)) || minfo$is_ordinal || minfo$is_multinomial) { # nolint
          out <- as.data.frame(lapply(updated_focal, function(i) {
            unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
              paste(unlist(contrast_terms[j, ]), collapse = "-")
            }))
          }), stringsAsFactors = FALSE)
        } else {
          # for "predictions()", we now have the row numbers. We can than extract
          # the factor levels from the data of the data grid, as row numbers in
          # "contrast_terms" correspond to rows in "grid".
          out <- as.data.frame(lapply(updated_focal, function(i) {
            unlist(lapply(seq_len(nrow(contrast_terms)), function(j) {
              .contrasts <- datagrid[[i]][as.numeric(unlist(contrast_terms[j, ]))]
              paste(.contrasts, collapse = "-")
            }))
          }), stringsAsFactors = FALSE)
        }
      }
      # the final result is a data frame with one column per focal predictor,
      # and the pairwise combinations of factor levels are the values
      colnames(out) <- updated_focal
    } else if (is.null(test)) {
      ## contrasts of group levels -----

      # we have simple contrasts - we can just copy from the data frame
      # returned by "marginaleffects" to get nice labels
      out <- as.data.frame(.comparisons[focal], stringsAsFactors = FALSE)
    } else {
      ## hypothesis testing of group levels -----

      # if we have specific comparisons of estimates, like "b1 = b2", we
      # want to replace these shortcuts with the full related predictor names
      # and levels
      if (any(grepl("b[0-9]+", .comparisons$term))) {
        # re-compute comoparisons for all combinations, so we know which
        # estimate refers to which combination of predictor levels
        if (need_average_predictions || margin %in% c("marginalmeans", "empirical")) {
          fun <- "avg_predictions"
        } else {
          fun <- "predictions"
        }
        fun_args <- list(
          model = object,
          variables = by_variables,
          newdata = datagrid,
          hypothesis = NULL,
          df = df,
          conf_level = ci_level
        )
        # for counterfactual predictions, we need no data grid
        if (margin == "empirical") {
          fun_args$newdata <- NULL
        }
        .full_comparisons <- .call_me(fun, fun_args, dot_args, include_random)

        # replace "hypothesis" labels with names/levels of focal predictors
        hypothesis_label <- .extract_labels(
          full_comparisons = .full_comparisons,
          focal = focal,
          test = test,
          old_labels = .comparisons$term
        )
      }
      # we have a specific hypothesis, like "b3 = b4". We just copy that information
      out <- data.frame(Hypothesis = .comparisons$term, stringsAsFactors = FALSE)
    }
    estimate_name <- ifelse(is.null(test), "Predicted", "Contrast")
  }

  # add result from equivalence test
  if (!is.null(rope_range)) {
    .comparisons <- marginaleffects::hypotheses(.comparisons, equivalence = rope_range, conf_level = ci_level)
    .comparisons$p.value <- .comparisons$p.value.equiv
  }

  # further results
  out[[estimate_name]] <- .comparisons$estimate
  out$conf.low <- .comparisons$conf.low
  out$conf.high <- .comparisons$conf.high
  out$p.value <- .comparisons$p.value

  # for pairwise comparisons, we may have comparisons inside one level when we
  # have multiple focal terms, like "1-1" and "a-b". In this case, the comparison
  # of 1 to 1 ("1-1") is just the contrast for the level "1", we therefore can
  # collpase that string
  if (isTRUE(collapse_levels)) {
    out <- .collapse_levels(out, datagrid, focal, by)
  }

  # replace back commas
  for (i in focal) {
    # in ".fix_comma_levels()", we replaced "," by "#*#", and now
    # we need to revert this, to preserve original level strings
    if (any(grepl("#*#", out[[i]], fixed = TRUE))) {
      out[[i]] <- gsub("#*#", ",", out[[i]], fixed = TRUE)
    }
  }

  # filter by-variables?
  if (!is.null(by)) {
    for (by_factor in by) {
      # values in "by" are character vectors, which are saved as "level-level".
      # we now extract the unique values, and filter the data frame
      unique_values <- unique(datagrid[[by_factor]])
      by_levels <- paste0(unique_values, "-", unique_values)
      keep_rows <- out[[by_factor]] %in% c(by_levels, unique_values)
      # filter final data frame
      out <- out[keep_rows, , drop = FALSE]
      # but we also need to filter the ".comparisons" data frame
      .comparisons <- .comparisons[keep_rows, , drop = FALSE]
      # finally, replace "level-level" just by "level"
      for (i in seq_along(by_levels)) {
        out[[by_factor]] <- gsub(
          by_levels[i],
          unique_values[i],
          out[[by_factor]],
          fixed = TRUE
        )
      }
    }
    # remove by-terms from focal terms
    focal <- focal[!focal %in% by]
  }

  # p-value adjustment?
  if (!is.null(p_adjust)) {
    out <- .p_adjust(out, p_adjust, datagrid, focal, .comparisons$statistic, df, verbose)
  }

  # add back response levels?
  if ("group" %in% colnames(.comparisons)) {
    out <- cbind(
      data.frame(response.level = .comparisons$group, stringsAsFactors = FALSE),
      out
    )
  } else if (minfo$is_ordinal || minfo$is_multinomial) {
    resp_levels <- levels(insight::get_response(object, verbose = FALSE))
    if (!is.null(resp_levels) && all(rowMeans(sapply(resp_levels, grepl, .comparisons$term, fixed = TRUE)) > 0)) { # nolint
      colnames(out)[seq_along(focal)] <- paste0("Response Level by ", paste0(focal, collapse = " & "))
      if (length(focal) > 1) {
        out[2:length(focal)] <- NULL
      }
    }
  }

  class(out) <- c("ggcomparisons", "data.frame")
  attr(out, "ci_level") <- ci_level
  attr(out, "test") <- test
  attr(out, "p_adjust") <- p_adjust
  attr(out, "df") <- df
  attr(out, "by_factor") <- by
  attr(out, "rope_range") <- rope_range
  attr(out, "scale") <- scale
  attr(out, "scale_label") <- .scale_label(minfo, scale)
  attr(out, "linear_model") <- minfo$is_linear
  attr(out, "hypothesis_label") <- hypothesis_label
  attr(out, "estimate_name") <- estimate_name
  attr(out, "msg_intervals") <- msg_intervals
  attr(out, "verbose") <- verbose
  attr(out, "engine") <- "marginaleffects"
  attr(out, "datagrid") <- datagrid
  attr(out, "standard_error") <- .comparisons$std.error
  attr(out, "link_inverse") <- .link_inverse(object, ...)
  attr(out, "link_function") <- insight::link_function(object)

  out
}


#' @rdname test_predictions
#' @export
test_predictions.ggeffects <- function(object,
                                       by = NULL,
                                       test = "pairwise",
                                       equivalence = NULL,
                                       scale = "response",
                                       p_adjust = NULL,
                                       df = NULL,
                                       collapse_levels = FALSE,
                                       engine = "marginaleffects",
                                       verbose = TRUE,
                                       ...) {
  # check for installed packages ----------------------------------------------
  # check if we have the appropriate package version installed
  # ---------------------------------------------------------------------------

  # default for "engine" argument?
  engine <- getOption("ggeffects_test_engine", engine)
  # validate "engine" argument
  engine <- .check_arg(engine, c("marginaleffects", "emmeans", "ggeffects"))

  if (!insight::check_if_installed("marginaleffects", quietly = TRUE) && engine == "marginaleffects") { # nolint
    engine <- "emmeans"
  }
  # if we don't have the package installed, we switch to emmeans
  if (!insight::check_if_installed("emmeans", quietly = TRUE) && engine == "emmeans") {
    engine <- "ggeffects"
  }

  # retrieve focal predictors
  focal <- attributes(object)$original.terms
  # retrieve ci level predictors
  ci_level <- attributes(object)$ci_level
  # check prediction type - we set the default scale here. This is only
  # required for models with zero-inflation component (see later)
  type <- attributes(object)$type

  # check if all focal terms are random effects - if so, we switch to ggeffects
  # because we cannot calculate comparisons for random effects with marginaleffects
  # or emmeans
  model <- .get_model_object(name = attr(object, "model.name", exact = TRUE))
  random_pars <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  if (!is.null(random_pars) && all(.clean_terms(focal) %in% random_pars) && !is.na(ci_level)) {
    engine <- "ggeffects"
  }

  # check if we have a glmmTMB zero-inflated model - if comparisons for the
  # zero-inflation probabilities are requedsted, we switch to ggeffects
  # because we cannot calculate them with marginaleffects or emmeans
  is_zero_inflated <- .safe(insight::model_info(model)$is_zero_inflated, FALSE)
  if (is_zero_inflated && inherits(model, "glmmTMB") && !is.null(type) && type %in% c("zi_prob", "zero", "zprob")) {
    engine <- "ggeffects"
  }

  # experimental! ------------------------------------------------------------
  # Not officially documented. This is currently work in progress and not
  # yet fully supported. The "ggeffects" engine is not yet fully implemented.
  # ---------------------------------------------------------------------------
  if (engine == "ggeffects") {
    return(.test_predictions_ggeffects(
      object,
      by = by,
      test = test,
      equivalence = equivalence,
      scale = scale,
      p_adjust = p_adjust,
      df = attributes(object)$df,
      ci_level = ci_level,
      collapse_levels = collapse_levels,
      verbose = verbose,
      ...
    ))
  }

  # information about vcov-matrix
  vcov_matrix <- attributes(object)$vcov
  # information about margin
  margin <- attributes(object)$margin

  # check prediction type for zero-inflated models - we can change scale here,
  # if it's still the default
  if (is_zero_inflated && scale == "response") {
    scale <- .get_zi_prediction_type(model, type)
  }

  dot_args <- list(...)
  # set default for marginaleffects, we pass this via dots
  if (!is.null(vcov_matrix) && is.null(dot_args$vcov)) {
    dot_args$vcov <- vcov_matrix
  }

  my_args <- list(
    object = model,
    terms = focal,
    by = by,
    test = test,
    equivalence = equivalence,
    scale = scale,
    p_adjust = p_adjust,
    df = df,
    ci_level = ci_level,
    collapse_levels = collapse_levels,
    margin = margin,
    engine = engine,
    verbose = verbose
  )

  do.call(test_predictions.default, c(my_args, dot_args))
}


# helper ------------------------


.call_me <- function(fun, fun_args, dot_args, include_random) {
  # concatenate all arguments
  all_args <- .compact_list(c(fun_args, dot_args))
  # since ".compact_list" removes NULL objects, we add it back for mixed models
  if (include_random) {
    all_args$re.form <- NULL
    # avoid message
    suppressMessages(suppressWarnings(do.call(get(fun, asNamespace("marginaleffects")), all_args)))
  } else {
    do.call(get(fun, asNamespace("marginaleffects")), all_args)
  }
}


.validate_by_argument <- function(by, datagrid) {
  # check for valid by-variable
  if (!is.null(by)) {
    # all by-terms need to be in data grid
    if (!all(by %in% colnames(datagrid))) {
      insight::format_error(
        paste0("Variable(s) `", toString(by[!by %in% colnames(datagrid)]), "` not found in data grid.")
      )
    }
    # by-terms must be categorical
    by_factors <- vapply(datagrid[by], is.factor, TRUE)
    if (!all(by_factors)) {
      insight::format_error(
        "All variables in `by` must be categorical.",
        paste0(
          "The following variables in `by` are not categorical: ",
          toString(paste0("`", by[!by_factors], "`"))
        )
      )
    }
  }
  by
}


.collapse_levels <- function(out, datagrid, focal, by) {
  # remove by-terms from focal terms
  if (!is.null(by)) {
    focal <- focal[!focal %in% by]
  }
  # iterate all focal terms, these are the column names in "out"
  for (i in focal) {
    flag_dash <- FALSE
    # for factors, we need to check whether factor levels contain "-"
    # if so, we need to replace it, else "strplit()" won't work"
    if (is.factor(datagrid[[i]])) {
      l <- levels(datagrid[[i]])
      dash_levels <- grepl("-", l, fixed = TRUE)
      if (any(dash_levels)) {
        for (j in l[dash_levels]) {
          # replace by a - hopefully - unique character, later revert
          out[[i]] <- gsub(j, gsub("-", "#~#", j, fixed = TRUE), out[[i]], fixed = TRUE)
          flag_dash <- TRUE
        }
      }
    }
    level_pairs <- strsplit(out[[i]], "-", fixed = TRUE)
    all_same <- vapply(level_pairs, function(j) {
      all(j == j[1])
    }, TRUE)
    if (any(all_same)) {
      out[[i]][all_same] <- vapply(level_pairs[all_same], unique, character(1))
    }
    # revert replacement
    if (flag_dash) {
      out[[i]] <- gsub("#~#", "-", out[[i]], fixed = TRUE)
      flag_dash <- FALSE
    }
  }
  out
}


.fix_comma_levels <- function(terms, datagrid, focal) {
  for (i in focal) {
    if (is.factor(datagrid[[i]])) {
      l <- levels(datagrid[[i]])
      comma_levels <- grepl(",", l, fixed = TRUE)
      if (any(comma_levels)) {
        for (j in l[comma_levels]) {
          # replace by a - hopefully - unique character, later revert
          terms <- gsub(j, gsub(",", "#*#", j, fixed = TRUE), terms, fixed = TRUE)
        }
      }
    }
  }
  terms
}


.extract_labels <- function(full_comparisons, focal, test, old_labels) {
  # now we have both names of predictors and their levels
  beta_rows <- full_comparisons[focal]
  beta_rows[] <- lapply(beta_rows, as.character)
  # extract coefficient numbers from "test" string, which are
  # equivalent to row numbers
  pos <- gregexpr("(b[0-9]+)", test)[[1]]
  len <- attributes(pos)$match.length
  row_number <- unlist(lapply(seq_along(pos), function(i) {
    substring(test, pos[i] + 1, pos[i] + len[i] - 1)
  }))
  # sort rownumbers, largest first. Else, we may have "b1" and "b13", and
  # if we replace "b1" by a label "foo", "b13" is also replaced and becomes
  # "foo3" (see #312)
  row_number <- row_number[order(as.numeric(row_number), decreasing = TRUE)]
  # loop through rows, and replace "b<d>" with related string
  for (i in row_number) {
    label <- paste0(
      colnames(beta_rows),
      paste0("[", as.vector(unlist(beta_rows[i, ], use.names = FALSE)), "]"),
      collapse = ","
    )
    old_labels <- gsub(paste0("b", i), label, old_labels, fixed = TRUE)
  }
  # remove whitespace around operators, but not inside brackets
  tokens <- c("=", "-", "\\+", "/", "\\*")
  replacements <- c("=", "-", "+", "/", "*")
  for (i in seq_along(tokens)) {
    pattern <- paste0(tokens[i], "(?![^\\[]*\\])")
    old_labels <- gsub(pattern, paste0(" ", replacements[i], " "), old_labels, perl = TRUE)
  }

  old_labels
}


.get_zi_prediction_type <- function(model, type) {
  # sanity check - for pooled predictions, we cannot retrieve the model
  if (!insight::is_model(model)) {
    return("response")
  }
  if (inherits(model, "glmmTMB")) {
    types <- c("conditional", "zprob")
  } else {
    types <- c("count", "zero")
  }
  switch(type,
    conditional = ,
    count = ,
    fixed = types[1],
    zi_prob = ,
    zero = ,
    zprob = types[2],
    "response"
  )
}


.scale_label <- function(minfo, scale) {
  scale_label <- NULL
  if (minfo$is_binomial || minfo$is_ordinal || minfo$is_multinomial) {
    scale_label <- switch(scale,
      response = "probabilities",
      link = "log-odds",
      oddsratios = "odds ratios",
      probs = ,
      probability = "probabilities",
      NULL
    )
  } else if (minfo$is_count) {
    scale_label <- switch(scale,
      response = "counts",
      link = "log-mean",
      irr = "incident rate ratios",
      count = ,
      conditional = "conditional means",
      zero = ,
      zprob = ,
      zi_prob = ,
      probs = ,
      probability = "probabilities",
      NULL
    )
  } else if (minfo$is_orderedbeta) {
    scale_label <- switch(scale,
      response = "proportions",
      link = "log-proportions",
      probs = ,
      probability = "probabilities",
      NULL
    )
  }
  scale_label
}


# p-value adjustment -------------------

.p_adjust <- function(params, p_adjust, datagrid, focal, statistic = NULL, df = Inf, verbose = TRUE) {
  # exit on NULL, or if no p-adjustment requested
  if (is.null(p_adjust) || identical(p_adjust, "none")) {
    return(params)
  }

  all_methods <- c(tolower(stats::p.adjust.methods), "tukey", "sidak")

  # needed for rank adjustment
  focal_terms <- datagrid[focal]
  rank_adjust <- prod(vapply(focal_terms, insight::n_unique, numeric(1)))

  # only proceed if valid argument-value
  if (tolower(p_adjust) %in% all_methods) {
    if (tolower(p_adjust) %in% tolower(stats::p.adjust.methods)) {
      # base R adjustments
      params$p.value <- stats::p.adjust(params$p.value, method = p_adjust)
    } else if (tolower(p_adjust) == "tukey") {
      if (!is.null(statistic)) {
        # tukey adjustment
        params$p.value <- suppressWarnings(stats::ptukey(
          sqrt(2) * abs(statistic),
          rank_adjust,
          df,
          lower.tail = FALSE
        ))
        # for specific contrasts, ptukey might fail, and the tukey-adjustement
        # could just be simple p-value calculation
        if (all(is.na(params$p.value))) {
          params$p.value <- 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE)
        }
      } else if (verbose) {
        insight::format_alert("No test-statistic found. P-values were not adjusted.")
      }
    } else if (tolower(p_adjust) == "sidak") {
      # sidak adjustment
      params$p.value <- 1 - (1 - params$p.value)^rank_adjust
    }
  } else if (verbose) {
    insight::format_alert(paste0("`p_adjust` must be one of ", toString(all_methods)))
  }
  params
}
