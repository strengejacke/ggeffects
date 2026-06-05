# Pool contrasts and comparisons from `test_predictions()`

This function "pools" (i.e. combines) multiple `ggcomparisons` objects,
returned by
[`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md),
in a similar fashion as
[`mice::pool()`](https://amices.org/mice/reference/pool.html).

## Usage

``` r
pool_comparisons(x, ...)
```

## Arguments

- x:

  A list of `ggcomparisons` objects, as returned by
  [`test_predictions()`](https://strengejacke.github.io/ggeffects/reference/test_predictions.md).

- ...:

  Currently not used.

## Value

A data frame with pooled comparisons or contrasts of predictions.

## Details

Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).

## References

Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New
York: John Wiley and Sons.

## Examples

``` r
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
comparisons <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  test_predictions(m, "age")
})
pool_comparisons(comparisons)
#> # Pairwise comparisons
#> 
#> age             | Contrast |       95% CI |      p
#> --------------------------------------------------
#> (20-39)-(40-59) |     5.84 |  1.47, 10.22 | < .001
#> (20-39)-(60-99) |     7.22 |  2.09, 12.34 | < .001
#> (40-59)-(60-99) |     1.37 | -2.79,  5.53 | 0.404 
```
