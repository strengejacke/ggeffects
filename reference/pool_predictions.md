# Pool Predictions or Estimated Marginal Means

This function "pools" (i.e. combines) multiple `ggeffects` objects, in a
similar fashion as
[`mice::pool()`](https://amices.org/mice/reference/pool.html).

## Usage

``` r
pool_predictions(x, ...)
```

## Arguments

- x:

  A list of `ggeffects` objects, as returned by
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md).

- ...:

  Currently not used.

## Value

A data frame with pooled predictions.

## Details

Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
Pooling is applied to the predicted values on the scale of the *linear
predictor*, not on the response scale, in order to have accurate pooled
estimates and standard errors. The final pooled predicted values are
then transformed to the response scale, using
[`insight::link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.html).
For binomial-logit predictions integrated over a normal random
intercept, pooling uses the marginal logit and its delta-method standard
error, holding each fitted intercept standard deviation fixed.

## References

Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New
York: John Wiley and Sons.

## Examples

``` r
# example for multiple imputed datasets
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
predictions <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  predict_response(m, "age")
})
pool_predictions(predictions)
#> # Predicted values of bmi
#> 
#> age   | Predicted |       95% CI
#> --------------------------------
#> 20-39 |     29.48 | 25.84, 33.11
#> 40-59 |     23.96 | 20.63, 27.29
#> 60-99 |     22.60 | 18.96, 26.23
#> 
#> Adjusted for:
#> * hyp =     no
#> * chl = 194.38
```
