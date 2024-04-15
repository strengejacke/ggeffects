#' @title (Pairwise) comparisons between predictions (marginal effects)
#' @name test_predictions2
#'
#' @description Function to test differences of adjusted predictions for
#'   statistical significance. This is usually called contrasts or (pairwise)
#'   comparisons, or "marginal effects". `hypothesis_test()` is an alias.
#'
#' @param model A fitted model object, or an object of class `ggeffects`.
#' @param test Hypothesis to test. By default, pairwise-comparisons are
#'   conducted. See section _Introduction into contrasts and pairwise comparisons_.
#'   If `engine = "emmeans"`, the `test` argument can also be `"interaction"`,
#'   to calculate interaction contrasts (difference-in-difference contrasts).
#' @param terms Character vector with the names of the focal terms from `model`,
#'   for which contrasts or comparisons should be displayed. At least one term
#'   is required, maximum length is three terms. If the first focal term is numeric,
#'   contrasts or comparisons for the *slopes* of this numeric predictor are
#'   computed (possibly grouped by the levels of further categorical focal
#'   predictors).
#' @param by Character vector specifying the names of predictors to condition on.
#'   Hypothesis test is then carried out for focal terms by each level of `by`
#'   variables. This is useful especially for interaction terms, where we want
#'   to test the interaction within "groups". `by` is only relevant for
#'   categorical predictors.
#' @param margin Character string, indicates the method how to marginalize over
#'   non-focal terms. See [`predict_response()`] for details. If `model` is an
#'   object of class `ggeffects`, the same `margin` argument is used as for the
#'   predictions.
#' @param scale Character string, indicating the scale on which the contrasts
#'   or comparisons are represented. Can be one of:
#'
#'   - `"response"` (default), which would return contrasts on the response
#'     scale (e.g. for logistic regression, as probabilities);
#'   - `"link"` to return contrasts on scale of the linear predictors
#'     (e.g. for logistic regression, as log-odds);
#'   - `"probability"` (or `"probs"`) returns contrasts on the probability scale,
#'     which is required for some model classes, like `MASS::polr()`;
#'   - `"oddsratios"` to return contrasts on the odds ratio scale (only applies
#'     to logistic regression models);
#'   - `"irr"` to return contrasts on the odds ratio scale (only applies to
#'     count models);
#'   - or a transformation function like `"exp"` or `"log"`, to return transformed
#'     (exponentiated respectively logarithmic) contrasts; note that these
#'     transformations are applied to the _response scale_.
#'
#'   **Note:** If the `scale` argument is not supported by the provided `model`,
#'   it is automatically changed to a supported scale-type (a message is printed
#'   when `verbose = TRUE`).
#' @param equivalence ROPE's lower and higher bounds. Should be `"default"` or
#'   a vector of length two (e.g., `c(-0.1, 0.1)`). If `"default"`,
#'   [`bayestestR::rope_range()`] is used. Instead of using the `equivalence`
#'   argument, it is also possible to call the `equivalence_test()` method
#'   directly. This requires the **parameters** package to be loaded. When
#'   using `equivalence_test()`, two more columns with information about the
#'   ROPE coverage and decision on H0 are added. Furthermore, it is possible
#'   to `plot()` the results from `equivalence_test()`. See
#'   [`bayestestR::equivalence_test()`] resp. [`parameters::equivalence_test.lm()`]
#'   for details.
#' @param p_adjust Character vector, if not `NULL`, indicates the method to
#'   adjust p-values. See [`stats::p.adjust()`] or [`stats::p.adjust.methods`]
#'   for details. Further possible adjustment methods are `"tukey"` or `"sidak"`,
#'   and for `johnson_neyman()`, `"fdr"` (or `"bh"`) and `"esarey"` (or its
#'   short-cut `"es"`) are available options. Some caution is necessary when
#'   adjusting p-value for multiple comparisons. See also section _P-value adjustment_
#'   below.
#' @param df Degrees of freedom that will be used to compute the p-values and
#'   confidence intervals. If `NULL`, degrees of freedom will be extracted from
#'   the model using [`insight::get_df()`] with `type = "wald"`.
#' @param ci_level Numeric, the level of the confidence intervals.
#' @param collapse_levels Logical, if `TRUE`, term labels that refer to identical
#'   levels are no longer separated by "-", but instead collapsed into a unique
#'   term label (e.g., `"level a-level a"` becomes `"level a"`). See 'Examples'.
#' @param engine Character string, indicates the package to use for computing
#' contrasts and comparisons. Can be either `"marginaleffects"` (default) or
#' `"emmeans"`. The latter is useful when the _marginaleffects_ package is not
#' available, or when the _emmeans_ package is preferred. Note that using
#' _emmeans_ as backend is currently not as feature rich as the default
#' (_marginaleffects_) and still in development.
#' @param verbose Toggle messages and warnings.
#' @param ci.lvl Deprecated, please use `ci_level`.
#' @param ... Arguments passed down to [`data_grid()`] when creating the reference
#'   grid and to [`marginaleffects::predictions()`] resp. [`marginaleffects::slopes()`].
#'   For instance, arguments `type` or `transform` can be used to back-transform
#'   comparisons and contrasts to different scales. `vcov` can be used to
#'   calculate heteroscedasticity-consistent standard errors for contrasts.
#'   See examples at the bottom of
#'   [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_comparisons_1.html)
#'   for further details. To define a heteroscedasticity-consistent
#'   variance-covariance matrix, you can either use the same arguments as for
#'   `predict_response()` etc., namely `vcov_fun`, `vcov_type` and `vcov_args`.
#'   These are then transformed into a matrix and passed down to the `vcov`
#'   argument in *marginaleffects*. Or you directly use the `vcov` argument. See
#'   `?marginaleffects::slopes` for further details.
#'
#' @seealso There is also an `equivalence_test()` method in the **parameters**
#'   package ([`parameters::equivalence_test.lm()`]), which can be used to
#'   test contrasts or comparisons for practical equivalence. This method also
#'   has a `plot()` method, hence it is possible to do something like:
#'   ```
#'   library(parameters)
#'   predict_response(model, focal_terms) |>
#'     equivalence_test() |>
#'     plot()
#'  ```
#'
#' @section Introduction into contrasts and pairwise comparisons:
#'
#' There are many ways to test contrasts or pairwise comparisons. A
#' detailed introduction with many (visual) examples is shown in
#' [this vignette](https://strengejacke.github.io/ggeffects/articles/introduction_comparisons_1.html).
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
#' `ggeffects_test_engine` can be used as option to either use the _marginaleffects_
#' package for computing contrasts and comparisons (default), or the _emmeans_
#' package (e.g. `options(ggeffects_test_engine = "emmeans")`). The latter is
#' useful when the _marginaleffects_ package is not available, or when the
#' _emmeans_ package is preferred. You can also provide the engine directly, e.g.
#' `test_predictions(..., engine = "emmeans")`. Note that using _emmeans_ as
#' backend is currently not as feature rich as the default (_marginaleffects_)
#' and still in development.
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
test_predictions2 <- function(model, ...) {
  UseMethod("test_predictions2")
}


#' @rdname test_predictions2
#' @export
test_predictions2.default <- function(model,
                                      terms = NULL,
                                      by = NULL,
                                      test = "pairwise",
                                      equivalence = NULL,
                                      scale = "response",
                                      p_adjust = NULL,
                                      df = NULL,
                                      ci_level = 0.95,
                                      collapse_levels = FALSE,
                                      margin = "mean_reference",
                                      engine = "ggeffects",
                                      verbose = TRUE,
                                      ...) {
  test_predictions(
    predict_response(
      model,
      terms = terms,
      ci_level = ci_level,
      margin = margin
    ),
    by = by,
    test = test,
    equivalence = equivalence,
    scale = scale,
    p_adjust = p_adjust,
    df = df,
    ci_level = ci_level,
    collapse_levels = collapse_levels,
    engine = engine,
    verbose = verbose,
    ...
  )
}

#' @export
test_predictions2.ggeffects <- function(model,
                                        by = NULL,
                                        test = "pairwise",
                                        equivalence = NULL,
                                        scale = "response",
                                        p_adjust = NULL,
                                        df = NULL,
                                        ci_level = 0.95,
                                        collapse_levels = FALSE,
                                        engine = "ggeffects",
                                        verbose = TRUE,
                                        ...) {
  pr_data <- as.data.frame(model, terms_to_colnames = TRUE)

  # some attributes we need
  focal_terms <- attributes(model)$terms
  at_list <- attributes(model)$at.list
  dof <- attributes(model)$df
  model <- .get_model_object(model)

  # contrasts
  if (is.null(test) || test == "contrasts") {
    pr_data$statistic <- pr_data$predicted / pr_data$std.error
    pr_data$p.value <- 2 * stats::pt(abs(pr_data$tatistic), df = dof, lower.tail = FALSE)
    pairwise_comparisons <- pr_data
  }

  if (test == "pairwise") {
    # create pairwise combinations
    level_pairs <- interaction(expand.grid(at_list))
    M <- matrix(
      1,
      nrow = length(level_pairs),
      ncol = length(level_pairs),
      dimnames = list(level_pairs, level_pairs)
    )
    M[!upper.tri(M)] <- NA
    pairs_data <- stats::na.omit(as.data.frame(as.table(M)))
    pairs_data$Freq <- NULL
    pairs_data <- lapply(pairs_data, as.character)

    out <- lapply(pairs_data, function(i) {
      pair <- strsplit(i, ".", fixed = TRUE)
      datawizard::data_rotate(as.data.frame(pair))
    })

    final <- do.call(rbind, lapply(seq_len(nrow(out[[1]])), function(i) {
      pos1 <- pr_data[[focal_terms[1]]] == out[[1]][i, 1]
      pos2 <- pr_data[[focal_terms[1]]] == out[[2]][i, 1]

      if (length(focal_terms) > 1) {
        pos1 <- pos1 & pr_data[[focal_terms[2]]] == out[[1]][i, 2]
        pos2 <- pos2 & pr_data[[focal_terms[2]]] == out[[2]][i, 2]
      }
      if (length(focal_terms) > 2) {
        pos1 <- pos1 & pr_data[[focal_terms[3]]] == out[[1]][i, 3]
        pos2 <- pos2 & pr_data[[focal_terms[3]]] == out[[2]][i, 3]
      }

      predicted1 <- pr_data$predicted[pos1]
      predicted2 <- pr_data$predicted[pos2]

      result <- as.data.frame(do.call(cbind, lapply(seq_along(focal_terms), function(j) {
        paste(out[[1]][i, j], out[[2]][i, j], sep = "-")
      })))

      colnames(result) <- focal_terms
      result$Contrast <- predicted1 - predicted2
      result$SE <- sqrt(pr_data$std.error[pos1]^2 + pr_data$std.error[pos2]^2)
      result
    }))
    final$CI_low <- final$Contrast - stats::qt(0.975, df = dof) * final$SE
    final$CI_high <- final$Contrast + stats::qt(0.975, df = dof) * final$SE
    final$Statistic <- final$Contrast / final$SE
    final$p <- 2 * stats::pt(abs(final$Statistic), df = dof, lower.tail = FALSE)
    pairwise_comparisons <- final
  }

  class(pairwise_comparisons) <- c("ggcomparisons", "data.frame")
  attr(pairwise_comparisons, "ci_level") <- ci_level
  attr(pairwise_comparisons, "test") <- test
  attr(pairwise_comparisons, "p_adjust") <- p_adjust
  attr(pairwise_comparisons, "df") <- df
  attr(pairwise_comparisons, "verbose") <- verbose
  attr(pairwise_comparisons, "standard_error") <- pairwise_comparisons$std.error
  attr(pairwise_comparisons, "link_inverse") <- insight::link_inverse(model)
  attr(pairwise_comparisons, "link_function") <- insight::link_function(model)
  pairwise_comparisons
}
# model <- ggpredict(model2, c("episode [1,2]", "grp"))

# f1 <- 1:3
# f2 <- c("control", "treatment")

# f12 <- interaction(expand.grid(f1, f2))

# M <- matrix(1, nrow = length(f12), ncol = length(f12), dimnames = list(f12, f12))
# M[!upper.tri(M)] <- NA

# x <- as.table(M) |>
#   as.data.frame() |>
#   na.omit()

# x <- lapply(x[1:2], as.character)
# out <- lapply(x[1:2], function(i) {
#   out <- strsplit(i, ".", fixed = TRUE)
#   datawizard::data_rotate(as.data.frame(out))
# })

# row.names(out) <- NULL

# preds <- as.data.frame(pr, terms_to_colnames = TRUE)

# pp <- do.call(rbind, lapply(1:nrow(out[[1]]), function(i) {
#   pos1 <- preds$episode == out[[1]][i, 1] & preds$grp == out[[1]][i, 2]
#   pos2 <- preds$episode == out[[2]][i, 1] & preds$grp == out[[2]][i, 2]
#   pp1 <- preds$predicted[pos1]
#   pp2 <- preds$predicted[pos2]
#   data.frame(
#     episode = paste0(out[[1]][i, 1], "-", out[[2]][i, 1]),
#     grp = paste0(out[[1]][i, 2], "-", out[[2]][i, 2]),
#     Contrast = pp1 - pp2,
#     SE = sqrt(preds$std.error[pos1]^2 + preds$std.error[pos2]^2)
#   )
# }))

# d1 <- datawizard::data_arrange(pp, "episode")
# d2 <- datawizard::data_arrange(test_predictions(model2, c("episode", "grp")), "episode")

# d1$CI_low <- d1$Contrast - qt(0.975, df = insight::get_df(model2)) * d1$SE
# d1$CI_high <- d1$Contrast + qt(0.975, df = insight::get_df(model2)) * d1$SE
# d1$Statistic <- d1$Contrast / d1$SE
# d1$p <- 2 * stats::pt(abs(d1$Statistic), df = insight::get_df(model2), lower.tail = FALSE)

# insight::format_table(d1)
# d2


# d1
# test_predictions(model2, c("episode", "grp"), engine = "emmeans", test = "interaction")

# parameters:::p_value.default()






# f1 <- 1:3
# f2 <- c("control", "treatment")
# f3 <- letters[1:3]

# f123 <- interaction(expand.grid(f1, f2, f3))

# M <- matrix(1, nrow = length(f123), ncol = length(f123), dimnames = list(f123, f123))
# M[!upper.tri(M)] <- NA

# x <- as.table(M) |>
#   as.data.frame() |>
#   na.omit()

# x <- lapply(x[1:2], as.character)
# out <- lapply(x[1:2], function(i) {
#   out <- strsplit(i, ".", fixed = TRUE)
#   datawizard::data_rotate(as.data.frame(out))
# })

# row.names(out) <- NULL

# do.call(rbind, lapply(1, function(i) {
#   paste(out[[1]][, i], out[[1]][, i], sep = "-")
# }))



# f1 <- 1:3
# f2 <- c("control", "treatment")
# f12 <- interaction(expand.grid(f1, f2))

# M <- matrix(1, nrow = length(f12), ncol = length(f12), dimnames = list(f12, f12))
# M[!upper.tri(M)] <- NA

# x <- as.table(M) |>
#   as.data.frame() |>
#   na.omit()

# x <- lapply(x[1:2], as.character)
# out <- lapply(x[1:2], function(i) {
#   out <- strsplit(i, ".", fixed = TRUE)
#   datawizard::data_rotate(as.data.frame(out))
# })

# row.names(out) <- NULL
# as.data.frame(do.call(cbind, lapply(1:2, function(i) {
#   paste(out[[1]][, i], out[[2]][, i], sep = "-")
# })))
