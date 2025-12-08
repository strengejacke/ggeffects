# Collapse raw data by random effect groups

This function extracts the raw data points (i.e. the data that was used
to fit the model) and "averages" (i.e. "collapses") the response
variable over the levels of the grouping factor given in `collapse_by`.
Only works with mixed models.

## Usage

``` r
collapse_by_group(grid, model, collapse_by = NULL, residuals = FALSE)
```

## Arguments

- grid:

  A data frame representing the data grid, or an object of class
  `ggeffects`, as returned by
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).

- model:

  The model for which to compute partial residuals. The data grid `grid`
  should match to predictors in the model.

- collapse_by:

  Name of the (random effects) grouping factor. Data is collapsed by the
  levels of this factor.

- residuals:

  Logical, if `TRUE`, collapsed partial residuals instead of raw data by
  the levels of the grouping factor.

## Value

A data frame with raw data points, averaged over the levels of the given
grouping factor from the random effects. The group level of the random
effect is saved in the column `"random"`.

## Examples

``` r
library(ggeffects)
data(efc, package = "ggeffects")
efc$e15relat <- as.factor(efc$e15relat)
efc$c161sex <- as.factor(efc$c161sex)
levels(efc$c161sex) <- c("male", "female")
model <- lme4::lmer(neg_c_7 ~ c161sex + (1 | e15relat), data = efc)
me <- predict_response(model, terms = "c161sex")
head(attributes(me)$rawdata)
#>   response x group facet panel rowname
#> 1       12 2     1     1     1       1
#> 2       20 2     1     1     1       2
#> 3       11 1     1     1     1       3
#> 4       10 1     1     1     1       4
#> 5       12 2     1     1     1       5
#> 6       19 1     1     1     1       6
collapse_by_group(me, model, "e15relat")
#>    x group_col facet panel random  response
#> 1  1         1     1     1      1 12.297872
#> 2  2         1     1     1      1 13.347107
#> 3  1         1     1     1      2 11.585586
#> 4  2         1     1     1      2 12.118310
#> 5  1         1     1     1      3 12.166667
#> 6  2         1     1     1      3 10.545455
#> 7  1         1     1     1      4 10.750000
#> 8  2         1     1     1      4 11.726027
#> 9  1         1     1     1      5 11.333333
#> 10 2         1     1     1      5 10.235294
#> 11 1         1     1     1      6  8.200000
#> 12 2         1     1     1      6  9.235294
#> 13 1         1     1     1      7 13.000000
#> 14 2         1     1     1      7 10.400000
#> 15 1         1     1     1      8  9.666667
#> 16 2         1     1     1      8 10.955882
```
