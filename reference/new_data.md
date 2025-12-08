# Create a data frame from all combinations of predictor values

Create a data frame for the "newdata"-argument that contains all
combinations of values from the terms in questions. Similar to
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html). The
`terms`-argument accepts all shortcuts for representative values as in
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).

## Usage

``` r
new_data(model, terms, typical = "mean", condition = NULL, ...)

data_grid(model, terms, typical = "mean", condition = NULL, ...)
```

## Arguments

- model:

  A fitted model object.

- terms:

  Character vector with the names of those terms from `model` for which
  all combinations of values should be created. This argument works in
  the same way as the `terms` argument in
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).
  See also [this
  vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html).

- typical:

  Character vector, naming the function to be applied to the covariates
  (non-focal terms) over which the effect is "averaged". The default is
  `"mean"`. Can be `"mean"`, "`weighted.mean`", `"median"`, `"mode"` or
  `"zero"`, which call the corresponding R functions (except `"mode"`,
  which calls an internal function to compute the most common value);
  `"zero"` simply returns 0. By default, if the covariate is a factor,
  only `"mode"` is applicable; for all other values (including the
  default, `"mean"`) the reference level is returned. For character
  vectors, only the mode is returned. You can use a named vector to
  apply different functions to integer, numeric and categorical
  covariates, e.g. `typical = c(numeric = "median", factor = "mode")`.
  If `typical` is `"weighted.mean"`, weights from the model are used. If
  no weights are available, the function falls back to `"mean"`.
  **Note** that this argument is ignored for
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md),
  because the `margin` argument takes care of this.

- condition:

  Named character vector, which indicates covariates that should be held
  constant at specific values. Unlike `typical`, which applies a
  function to the covariates to determine the value that is used to hold
  these covariates constant, `condition` can be used to define exact
  values, for instance `condition = c(covariate1 = 20, covariate2 = 5)`.
  See 'Examples'.

- ...:

  Currently not used.

## Value

A data frame containing one row for each combination of values of the
supplied variables.

## Examples

``` r
data(efc, package = "ggeffects")
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
new_data(fit, c("c12hour [meansd]", "c161sex"))
#>   c12hour c161sex  neg_c_7 c172code
#> 1    -8.3       1 11.83804 1.970552
#> 2    42.2       1 11.83804 1.970552
#> 3    92.7       1 11.83804 1.970552
#> 4    -8.3       2 11.83804 1.970552
#> 5    42.2       2 11.83804 1.970552
#> 6    92.7       2 11.83804 1.970552

nd <- new_data(fit, c("c12hour [meansd]", "c161sex"))
pr <- predict(fit, type = "response", newdata = nd)
nd$predicted <- pr
nd
#>   c12hour c161sex  neg_c_7 c172code predicted
#> 1    -8.3       1 11.83804 1.970552  76.75375
#> 2    42.2       1 11.83804 1.970552  63.96204
#> 3    92.7       1 11.83804 1.970552  51.17033
#> 4    -8.3       2 11.83804 1.970552  77.79518
#> 5    42.2       2 11.83804 1.970552  65.00347
#> 6    92.7       2 11.83804 1.970552  52.21175

# compare to
predict_response(fit, c("c12hour [meansd]", "c161sex"))
#> # Predicted values of Total score BARTHEL INDEX
#> 
#> c161sex: Male
#> 
#> c12hour | Predicted |       95% CI
#> ----------------------------------
#>   -8.30 |     76.75 | 73.02, 80.49
#>   42.20 |     63.96 | 60.57, 67.35
#>   92.70 |     51.17 | 47.30, 55.04
#> 
#> c161sex: Female
#> 
#> c12hour | Predicted |       95% CI
#> ----------------------------------
#>   -8.30 |     77.80 | 75.20, 80.39
#>   42.20 |     65.00 | 63.11, 66.90
#>   92.70 |     52.21 | 49.68, 54.74
#> 
#> Adjusted for:
#> *  neg_c_7 = 11.84
#> * c172code =  1.97
```
