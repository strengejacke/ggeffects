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
#' @param test Hypothesis to test, defined as character string, formula, or
#' data frame. Can be one of:
#'
#' * String:
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
#'
#' * String equation:
#'
#'   A character string with a custom hypothesis, e.g. `"b2 = b1"`. This would
#'   test if the second level of a predictor is different from the first level.
#'   Custom hypotheses are very flexible. It is also possible to test interaction
#'   contrasts (difference-in-difference contrasts) with custom hypotheses, e.g.
#'   `"(b2 - b1) = (b4 - b3)"`. See also section _Introduction into contrasts
#'   and pairwise comparisons_.
#'
#' * Formula:
#'
#'   A formula, where the left-hand side indicates the type of comparison and
#'   the right-hand side which pairs to compare. Optionally, grouping variables
#'   can be specified after a vertical bar. See also 'Examples'.
#'   - For the left-hand side, comparisons can be `difference` or `ratio`.
#'   - For the right-hand side, pairs can be `reference`, `sequential`, or
#'     `meandev`. For `reference`, all factor levels are compared to the
#'     reference level. `sequential` compares consecutive levels of a predictor.
#'     `meandev` compares each factor level against the "average" factor level.
#'   - If a variable is specified after `|`, comparisons will be grouped by
#'     that variable.
#'
#' * A data frame with custom contrasts. See 'Examples'.
#'
#' * `NULL`, in which case simple contrasts are computed.
#'
#' Technical details about the packages used as back-end to calculate contrasts
#' and pairwise comparisons are provided in the section _Packages used as back-end
#' to calculate contrasts and pairwise comparisons_ below.
#' @param test_args Optional arguments passed to `test`, typically provided as
#' named list. Only applies to those options that use the **emmeans** package
#' as backend, e.g. if `test = "interaction"`, `test_args` will be passed to
#' `emmeans::contrast(interaction = test_args)`. For other *emmeans* options
#' (like `"contrast"`, `"exclude"`, `"consecutive"` and so on), `test_args` will
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
#' (**marginaleffects**). Setting `engine = "emmeans"` provides some additional
#' test options: `"interaction"` to calculate interaction contrasts,
#' `"consecutive"` to calculate contrasts between consecutive levels of a
#' predictor, or a data frame with custom contrasts (see also `test`). There is
#' a third option as well, `engine = "ggeffects"`. However, this option offers
#' less features as the default engine, `"marginaleffects"`. It can be faster in
#' some cases, though, and works for comparing predicted random effects in mixed
#' models, or predicted probabilities of the zero-inflation component. If the
#' **marginaleffects** package is not installed, the **emmeans** package is used
#' automatically. If this package is not installed as well, `engine = "ggeffects"`
#' is used.
#' @param condition Named character vector, which indicates covariates that
#' should be held constant at specific values, for instance
#' `condition = c(covariate1 = 20, covariate2 = 5)`.
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
#'   - for formulas, the **marginaleffects** package is used.
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
#' backend is currently not as feature rich as the default (**marginaleffects**).
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
#' @examplesIf all(insight::check_if_installed(c("parameters", "marginaleffects", "modelbased"), quietly = TRUE)) && interactive()
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
#' # same as (using formula):
#' pr <- predict_response(m, c("time", "coffee"))
#' test_predictions(pr, test = difference ~ sequential | coffee)
#'
#' # interaction contrasts - difference-in-difference comparisons
#' pr <- predict_response(m, c("time", "coffee"), margin = "marginalmeans")
#' test_predictions(pr, test = "interaction")
#'
#' # Ratio contrasts ---------------------------------------
#' # -------------------------------------------------------
#' test_predictions(test = ratio ~ reference | coffee)
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
                                     margin = "mean_reference",
                                     condition = NULL,
                                     collapse_levels = FALSE,
                                     engine = "marginaleffects",
                                     verbose = TRUE,
                                     ...) {
  # margin-argument -----------------------------------------------------------
  # harmonize the "margin" argument
  # ---------------------------------------------------------------------------
  margin <- .tp_validate_margin(margin)

  # check engine and check for installed packages -----------------------------
  # check if we have the appropriate package version installed
  # ---------------------------------------------------------------------------
  engine <- .tp_validate_engine(engine, test)

  # object-argument -----------------------------------------------------------
  # validate the "object" argument
  # ---------------------------------------------------------------------------
  object <- .tp_validate_object(object)

  # ci-level cannot be NA, set to default value then
  if (is.na(ci_level)) {
    ci_level <- 0.95
  }
  miss_scale <- missing(scale)

  # dot-arguments -------------------------------------------------------------
  # evaluate dots - remove conflicting additional arguments. We have our own
  # "scale" argument that modulates the "type" and "transform" arguments
  # in "marginaleffects"
  # ---------------------------------------------------------------------------
  dot_args <- .tp_validate_dot_args(
    dot_args = list(...),
    scale = scale,
    object = object,
    miss_scale = miss_scale,
    verbose = verbose
  )

  # engine --------------------------------------------------------------------
  # here we switch to emmeans, if "engine" is set to "emmeans"
  # ---------------------------------------------------------------------------
  if (engine == "emmeans") {
    # warn about non-supported arguments
    if (!is.null(equivalence) && verbose) {
      insight::format_alert("Argument `equivalence` is not supported when `engine = \"emmeans\"`.")
    }
    return(.test_predictions_emmeans(
      object,
      terms = terms,
      by = by,
      test = test,
      test_args = test_args,
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

  marginalize <- switch(margin,
    mean_reference = ,
    mean_mode = "specific",
    marginalmeans = "average",
    "population"
  )

  # check if we have representative values in brackets
  representative_values <- .get_representative_values(
    terms,
    insight::get_data(object, source = "mf", verbose = FALSE)
  )

  # convert terms argument
  if (!is.null(representative_values)) {
    terms <- unlist(lapply(names(representative_values), function(i) {
      value <- representative_values[[i]]
      # for non numeric, surround with quotes
      if (anyNA(suppressWarnings(as.numeric(value)))) {
        value <- paste0("'", value, "'")
      }
      paste0(i, "=c(", paste(value, collapse = ","), ")")
    }), use.names = FALSE)
  }

  out <- modelbased::estimate_contrasts(
    object,
    contrast = terms,
    by = by,
    predict = dot_args$type,
    ci = ci_level,
    comparison = test,
    estimate = marginalize,
    p_adjust = p_adjust,
    equivalence = equivalence,
    ...
  )

  class(out) <- c(
    intersect(
      class(out),
      c("estimate_contrasts", "see_estimate_contrasts", "estimate_slopes", "see_estimate_slopes")
    ),
    "ggcomparisons",
    "data.frame"
  )

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
  engine <- insight::validate_argument(engine, c("marginaleffects", "emmeans", "ggeffects"))

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
  # retrieve condition argument
  condition <- attributes(object)$condition
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

  # if type = "simulate", we have to use the ggeffects-engine
  if (identical(type, "simulate")) {
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
    condition = condition,
    engine = engine,
    verbose = verbose
  )

  do.call(test_predictions.default, c(my_args, dot_args))
}


# helper ------------------------------

.tp_validate_margin <- function(margin) {
  # default for "margin" argument?
  margin <- getOption("ggeffects_margin", margin)
  # validate "margin" argument
  margin <- insight::validate_argument(
    margin,
    c(
      "mean_reference", "mean_mode", "marginalmeans", "empirical",
      "counterfactual", "full_data", "average", "marginaleffects"
    )
  )
  # harmonize argument
  switch(margin,
    mean_reference = ,
    mean_mode = "mean_reference",
    marginalmeans = "marginalmeans",
    "empirical"
  )
}


.tp_validate_engine <- function(engine, test) {
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

  engine
}


.tp_validate_object <- function(object) {
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

  object
}


.tp_validate_dot_args <- function(dot_args, scale, object, miss_scale, verbose) {
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

  dot_args
}
