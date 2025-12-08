# (Pairwise) comparisons between predictions (marginal effects)

Function to test differences of adjusted predictions for statistical
significance. This is usually called contrasts or (pairwise)
comparisons, or "marginal effects". `hypothesis_test()` is an alias.

## Usage

``` r
test_predictions(object, ...)

hypothesis_test(object, ...)

# Default S3 method
test_predictions(
  object,
  terms = NULL,
  by = NULL,
  test = "pairwise",
  test_args = NULL,
  p_adjust = NULL,
  df = NULL,
  ci_level = 0.95,
  margin = "mean_reference",
  condition = NULL,
  collapse_levels = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'ggeffects'
test_predictions(
  object,
  by = NULL,
  test = "pairwise",
  p_adjust = NULL,
  df = NULL,
  collapse_levels = FALSE,
  engine = "emmeans",
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  A fitted model object, or an object of class `ggeffects`. If `object`
  is of class `ggeffects`, arguments `terms`, `margin` and `ci_level`
  are taken from the `ggeffects` object and don't need to be specified.

- ...:

  Arguments passed down to
  [`data_grid()`](https://strengejacke.github.io/ggeffects/reference/new_data.md)
  when creating the reference grid.

- terms:

  If `object` is an object of class `ggeffects`, the same `terms`
  argument is used as for the predictions, i.e. `terms` can be ignored.
  Else, if `object` is a model object, `terms` must be a character
  vector with the names of the focal terms from `object`, for which
  contrasts or comparisons should be displayed. At least one term is
  required, maximum length is three terms. If the first focal term is
  numeric, contrasts or comparisons for the *slopes* of this numeric
  predictor are computed (possibly grouped by the levels of further
  categorical focal predictors).

- by:

  Character vector specifying the names of predictors to condition on.
  Hypothesis test is then carried out for focal terms by each level of
  `by` variables. This is useful especially for interaction terms, where
  we want to test the interaction within "groups". `by` is only relevant
  for categorical predictors.

- test:

  Hypothesis to test, defined as character string, formula, or data
  frame. Can be one of:

  - String:

    - `"pairwise"` (default), to test pairwise comparisons.

    - `"trend"` (or `"slope"`) to test for the linear trend/slope of
      (usually) continuous predictors. These options are just aliases
      for setting `trend = NULL`.

    - `"contrast"` to test simple contrasts (i.e. each level is tested
      against the average over *all* levels).

    - `"exclude"` to test simple contrasts (i.e. each level is tested
      against the average over *all other* levels, excluding the
      contrast that is being tested).

    - `"interaction"` to test interaction contrasts
      (difference-in-difference contrasts). More flexible interaction
      contrasts can be calcualted using the `test_args` argument.

    - `"consecutive"` to test contrasts between consecutive levels of a
      predictor.

    - `"polynomial"` to test orthogonal polynomial contrasts, assuming
      equally-spaced factor levels.

  - A data frame with custom contrasts. See 'Examples'.

  - `NULL`, in which case simple contrasts are computed.

- test_args:

  Optional arguments passed to `test`, typically provided as named list.
  Only applies to those options that use the **emmeans** package as
  backend, e.g. if `test = "interaction"`, `test_args` will be passed to
  `emmeans::contrast(interaction = test_args)`. For other *emmeans*
  options (like `"contrast"`, `"exclude"`, `"consecutive"` and so on),
  `test_args` will be passed to the `option` argument in
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

- p_adjust:

  Character vector, if not `NULL`, indicates the method to adjust
  p-values. See
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) or
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html) for
  details. Further possible adjustment methods are `"tukey"` or
  `"sidak"`. Some caution is necessary when adjusting p-value for
  multiple comparisons. See also section *P-value adjustment* below.

- df:

  Degrees of freedom that will be used to compute the p-values and
  confidence intervals. If `NULL`, degrees of freedom will be extracted
  from the model using
  [`insight::get_df()`](https://easystats.github.io/insight/reference/get_df.html)
  with `type = "wald"`.

- ci_level:

  Numeric, the level of the confidence intervals. If `object` is an
  object of class `ggeffects`, the same `ci_level` argument is used as
  for the predictions, i.e. `ci_level` can be ignored.

- margin:

  Character string, indicates the method how to marginalize over
  non-focal terms. See
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  for details. If `object` is an object of class `ggeffects`, the same
  `margin` argument is used as for the predictions, i.e. `margin` can be
  ignored.

- condition:

  Named character vector, which indicates covariates that should be held
  constant at specific values, for instance
  `condition = c(covariate1 = 20, covariate2 = 5)`.

- collapse_levels:

  Logical, if `TRUE`, term labels that refer to identical levels are no
  longer separated by "-", but instead collapsed into a unique term
  label (e.g., `"level a-level a"` becomes `"level a"`). See 'Examples'.

- verbose:

  Toggle messages and warnings.

- engine:

  Character string, indicates the package to use for computing contrasts
  and comparisons. Setting `engine = "emmeans"` (default) provides some
  additional test options: `"interaction"` to calculate interaction
  contrasts, `"consecutive"` to calculate contrasts between consecutive
  levels of a predictor, or a data frame with custom contrasts (see also
  `test`). The second option, `engine = "ggeffects"`. However, this
  option offers less features as the default engine, `"emmeans"`. It can
  be faster in some cases, though, and works for comparing predicted
  random effects in mixed models, or predicted probabilities of the
  zero-inflation component.

## Value

A data frame containing predictions (e.g. for `test = NULL`), contrasts
or pairwise comparisons of adjusted predictions or estimated marginal
means.

## Simple workflow for pairwise comparisons

A simple workflow includes calculating adjusted predictions and passing
the results directly to `test_predictions()`, e.g.:

    # 1. fit your model
    model <- lm(mpg ~ hp + wt + am, data = mtcars)
    # 2. calculate adjusted predictions
    pr <- predict_response(model, "am")
    pr
    # 3. test pairwise comparisons
    test_predictions(pr)

See also [this
vignette](https://strengejacke.github.io/ggeffects/articles/practical_glm_workflow.html).

## P-value adjustment for multiple comparisons

Note that p-value adjustment for methods supported by
[`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) (see also
`p.adjust.methods`), each row is considered as one set of comparisons,
no matter which `test` was specified. That is, for instance, when
`test_predictions()` returns eight rows of predictions (when
`test = NULL`), and `p_adjust = "bonferroni"`, the p-values are adjusted
in the same way as if we had a test of pairwise comparisons
(`test = "pairwise"`) where eight rows of comparisons are returned. For
methods `"tukey"` or `"sidak"`, a rank adjustment is done based on the
number of combinations of levels from the focal predictors in `terms`.
Thus, the latter two methods may be useful for certain tests only, in
particular pairwise comparisons.

## Global Options to Customize Tables when Printing

The `verbose` argument can be used to display or silence messages and
warnings. Furthermore,
[`options()`](https://rdrr.io/r/base/options.html) can be used to set
defaults for the
[`print()`](https://strengejacke.github.io/ggeffects/reference/print.md)
and
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
method. The following options are available, which can simply be run in
the console:

- `ggeffects_ci_brackets`: Define a character vector of length two,
  indicating the opening and closing parentheses that encompass the
  confidence intervals values, e.g.
  `options(ggeffects_ci_brackets = c("[", "]"))`.

- `ggeffects_collapse_ci`: Logical, if `TRUE`, the columns with
  predicted values (or contrasts) and confidence intervals are collapsed
  into one column, e.g. `options(ggeffects_collapse_ci = TRUE)`.

- `ggeffects_collapse_p`: Logical, if `TRUE`, the columns with predicted
  values (or contrasts) and p-values are collapsed into one column, e.g.
  `options(ggeffects_collapse_p = TRUE)`. Note that p-values are
  replaced by asterisk-symbols (stars) or empty strings when
  `ggeffects_collapse_p = TRUE`, depending on the significance level.

- `ggeffects_collapse_tables`: Logical, if `TRUE`, multiple tables for
  subgroups are combined into one table. Only works when there is more
  than one focal term, e.g. `options(ggeffects_collapse_tables = TRUE)`.

- `ggeffects_output_format`: String, either `"text"`, `"markdown"` or
  `"html"`. Defines the default output format from
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).
  If `"html"`, a formatted HTML table is created and printed to the view
  pane. `"markdown"` creates a markdown-formatted table inside Rmarkdown
  documents, and prints a text-format table to the console when used
  interactively. If `"text"` or `NULL`, a formatted table is printed to
  the console, e.g. `options(ggeffects_output_format = "html")`.

- `ggeffects_html_engine`: String, either `"tt"` or `"gt"`. Defines the
  default engine to use for printing HTML tables. If `"tt"`, the
  *tinytable* package is used, if `"gt"`, the *gt* package is used, e.g.
  `options(ggeffects_html_engine = "gt")`.

Use `options(<option_name> = NULL)` to remove the option.

## References

Esarey, J., & Sumner, J. L. (2017). Marginal effects in interaction
models: Determining and controlling the false positive rate. Comparative
Political Studies, 1–33. Advance online publication. doi:
10.1177/0010414017730080

## See also

There is also an `equivalence_test()` method in the **parameters**
package
([`parameters::equivalence_test.lm()`](https://easystats.github.io/parameters/reference/equivalence_test.lm.html)),
which can be used to test contrasts or comparisons for practical
equivalence. This method also has a
[`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
method, hence it is possible to do something like:

    library(parameters)
    predict_response(model, focal_terms) |>
      equivalence_test() |>
      plot()

## Examples

``` r
if (FALSE) { # all(insight::check_if_installed(c("parameters", "emmeans"), quietly = TRUE)) && interactive()
# \donttest{
data(efc)
efc$c172code <- as.factor(efc$c172code)
efc$c161sex <- as.factor(efc$c161sex)
levels(efc$c161sex) <- c("male", "female")
m <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

# direct computation of comparisons
test_predictions(m, "c172code")

# passing a `ggeffects` object
pred <- predict_response(m, "c172code")
test_predictions(pred)

# test for slope
test_predictions(m, "c12hour")

# interaction - contrasts by groups
m <- lm(barthtot ~ c12hour + c161sex * c172code + neg_c_7, data = efc)
test_predictions(m, c("c161sex", "c172code"), test = NULL)

# interaction - pairwise comparisons by groups
test_predictions(m, c("c161sex", "c172code"))

# interaction - collapse unique levels
test_predictions(m, c("c161sex", "c172code"), collapse_levels = TRUE)

# p-value adjustment
test_predictions(m, c("c161sex", "c172code"), p_adjust = "tukey")

# not all comparisons, only by specific group levels
test_predictions(m, "c172code", by = "c161sex")

# interaction - slope by groups
m <- lm(barthtot ~ c12hour + neg_c_7 * c172code + c161sex, data = efc)
test_predictions(m, c("neg_c_7", "c172code"))

# Interaction and consecutive contrasts -----------------
# -------------------------------------------------------
data(coffee_data, package = "ggeffects")
m <- lm(alertness ~ time * coffee + sex, data = coffee_data)

# consecutive contrasts
test_predictions(m, "time", by = "coffee", test = "consecutive")

# same as (using formula):
pr <- predict_response(m, c("time", "coffee"))
test_predictions(pr, test = difference ~ sequential | coffee)

# interaction contrasts - difference-in-difference comparisons
pr <- predict_response(m, c("time", "coffee"), margin = "marginalmeans")
test_predictions(pr, test = "interaction")

# Ratio contrasts ---------------------------------------
# -------------------------------------------------------
test_predictions(test = ratio ~ reference | coffee)

# Custom contrasts --------------------------------------
# -------------------------------------------------------
wakeup_time <- data.frame(
  "wakeup vs later" = c(-2, 1, 1) / 2, # make sure each "side" sums to (+/-)1!
  "start vs end of day" = c(-1, 0, 1)
)
test_predictions(m, "time", by = "coffee", test = wakeup_time)
# }
}
```
