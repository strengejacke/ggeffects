# Compute partial residuals from a data grid

This function computes partial residuals based on a data grid, where the
data grid is usually a data frame from all combinations of factor
variables or certain values of numeric vectors. This data grid is
usually used as `newdata` argument in
[`predict()`](https://rdrr.io/r/stats/predict.html), and can be created
with
[`new_data()`](https://strengejacke.github.io/ggeffects/reference/new_data.md).

## Usage

``` r
residualize_over_grid(grid, model, ...)

# S3 method for class 'data.frame'
residualize_over_grid(grid, model, predictor_name, ...)

# S3 method for class 'ggeffects'
residualize_over_grid(grid, model, protect_names = TRUE, ...)
```

## Arguments

- grid:

  A data frame representing the data grid, or an object of class
  `ggeffects`, as returned by
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).

- model:

  The model for which to compute partial residuals. The data grid `grid`
  should match to predictors in the model.

- ...:

  Currently not used.

- predictor_name:

  The name of the focal predictor, for which partial residuals are
  computed.

- protect_names:

  Logical, if `TRUE`, preserves column names from the `ggeffects`
  objects that is used as `grid`.

## Value

A data frame with residuals for the focal predictor.

## Partial Residuals

For **generalized linear models** (glms), residualized scores are
computed as `inv.link(link(Y) + r)` where `Y` are the predicted values
on the response scale, and `r` are the *working* residuals.\
\
For (generalized) linear **mixed models**, the random effect are also
partialled out.

## References

Fox J, Weisberg S. Visualizing Fit and Lack of Fit in Complex Regression
Models with Predictor Effect Plots and Partial Residuals. Journal of
Statistical Software 2018;87.

## Examples

``` r
library(ggeffects)
set.seed(1234)
x <- rnorm(200)
z <- rnorm(200)
# quadratic relationship
y <- 2 * x + x^2 + 4 * z + rnorm(200)

d <- data.frame(x, y, z)
model <- lm(y ~ x + z, data = d)

pr <- predict_response(model, c("x [all]", "z"))
head(residualize_over_grid(pr, model))
#>          x group predicted
#> 53  -1.207  0.07 -1.797239
#> 402  0.277  1.08  4.888712
#> 518  1.084  0.07  3.232202
#> 9   -2.346  1.08  4.133561
#> 428  0.429  0.07  1.801594
#> 441  0.506  1.08  5.659527
```
