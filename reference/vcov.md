# Calculate variance-covariance matrix for adjusted predictions

Returns the variance-covariance matrix for the predicted values from
`object`.

## Usage

``` r
# S3 method for class 'ggeffects'
vcov(object, vcov = NULL, vcov_args = NULL, verbose = TRUE, ...)
```

## Arguments

- object:

  An object of class `"ggeffects"`, as returned by
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for confidence intervals based on robust standard errors). This
  argument accepts a covariance matrix, a function which returns a
  covariance matrix, or a string which identifies the function to be
  used to compute the covariance matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)

    - Cluster-robust: `"vcovCR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md).

    - Bootstrap: `"BS"`, `"xy"`, `"fractional"`, `"jackknife"`,
      `"residual"`, `"wild"`, `"mammen"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://zeileis.codeberg.page/sandwich/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`, or
      `"PL"`.

  If `NULL`, standard errors (and confidence intervals) for predictions
  are based on the standard errors as returned by the
  [`predict()`](https://rdrr.io/r/stats/predict.html)-function. **Note**
  that probably not all model objects that work with
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  are also supported by the **sandwich** or **clubSandwich** packages.

  See details in [this
  vignette](https://strengejacke.github.io/ggeffects/articles/practical_robustestimation.html).

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://zeileis.codeberg.page/sandwich/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"` the default is
  set to `"CR3"`. For other defaults, refer to the documentation in the
  **sandwich** or **clubSandwich** package.

- verbose:

  Toggle messages or warnings.

- ...:

  Currently not used.

## Value

The variance-covariance matrix for the predicted values from `object`.

## Details

The returned matrix has as many rows (and columns) as possible
combinations of predicted values from the
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
call. For example, if there are two variables in the `terms`-argument of
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
with 3 and 4 levels each, there will be 3\*4 combinations of predicted
values, so the returned matrix has a 12x12 dimension. In short,
`nrow(object)` is always equal to `nrow(vcov(object))`. See also
'Examples'.

## Examples

``` r
data(efc)
model <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
result <- predict_response(model, c("c12hour [meansd]", "c161sex"))

vcov(result)
#>             1           2            3            4           5           6
#> 1  3.62642428  0.71518593  2.922676591  0.011438238  2.21892890 -0.69230945
#> 2  0.71518593  1.74243837 -0.073477976  0.953774462 -0.86214188  0.16511056
#> 3  2.92267659 -0.07347798  2.987294098 -0.008860469  3.05191160  0.05575704
#> 4  0.01143824  0.95377446 -0.008860469  0.933475755 -0.02915918  0.91317705
#> 5  2.21892890 -0.86214188  3.051911604 -0.029159177  3.88489431  0.80382353
#> 6 -0.69230945  0.16511056  0.055757037  0.913177047  0.80382353  1.66124354

# compare standard errors
sqrt(diag(vcov(result)))
#>         1         2         3         4         5         6 
#> 1.9043173 1.3200145 1.7283790 0.9661655 1.9710135 1.2888924 
as.data.frame(result)
#>      x predicted std.error conf.low conf.high  group
#> 1 -8.3  76.75375 1.9043173 73.01577  80.49173   Male
#> 2 -8.3  77.79518 1.3200145 75.20412  80.38623 Female
#> 3 42.2  63.96204 1.7283790 60.56941  67.35467   Male
#> 4 42.2  65.00347 0.9661655 63.10698  66.89995 Female
#> 5 92.7  51.17033 1.9710135 47.30143  55.03923   Male
#> 6 92.7  52.21175 1.2888924 49.68179  54.74172 Female

# only two predicted values, no further terms
# vcov() returns a 2x2 matrix
result <- predict_response(model, "c161sex")
vcov(result)
#>              1            2
#> 1  2.987290958 -0.008861546
#> 2 -0.008861546  0.933476742

# 2 levels for c161sex multiplied by 3 levels for c172code
# result in 6 combinations of predicted values
# thus vcov() returns a 6x6 matrix
result <- predict_response(model, c("c161sex", "c172code"))
vcov(result)
#>             1            2           3           4            5           6
#> 1  4.87803294  3.024738355  1.17144377  1.76091479 -0.092379792 -1.94567437
#> 2  3.02473835  2.983277938  2.94181752  0.03225611 -0.009204306 -0.05066472
#> 3  1.17144377  2.941817522  4.71219127 -1.69640257  0.073971180  1.84434493
#> 4  1.76091479  0.032256111 -1.69640257  2.58228744  0.853628756 -0.87502993
#> 5 -0.09237979 -0.009204306  0.07397118  0.85362876  0.936804241  1.01997973
#> 6 -1.94567437 -0.050664722  1.84434493 -0.87502993  1.019979727  2.91498938
```
