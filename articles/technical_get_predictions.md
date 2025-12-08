# Technical Details: Adding own class-methods to ggeffects

[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
is the core function to return adjusted predictions for a model, when
calling
[`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
or
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
with `margin = "mean_reference"` (the default option for `margin`).
Basically, the input contains the model object and a data grid that is
typically used for the `newdata` argument of the
[`predict()`](https://rdrr.io/r/stats/predict.html) method.
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
can be used as S3-method for own classes, to add support for new models
in **ggeffects** and is only relevant for package developers.

There are no S3-class definitions for
[`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
or
[`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
because these functions simply call methods from the **emmeans** or
**marginaleffects** packages. Hence, methods should be written for those
packages, too, if a model-object should work with
[`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
or
[`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

## Input: Arguments that are passed to `get_predictions()`

Adding support for
[`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
(or:
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
with default options) is quite easy. In order to make your model class
work with
[`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
you need to add a
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
method.
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
is called from
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
and following arguments are *always* passed to
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md) -
no matter if the corresponding class requires all of those arguments or
not:

- model, data_grid, terms, ci_level, model_info, type, typical, vcov,
  vcov_args, condition, interval, link_inverse, bias_correction, verbose

Please refer to the documentation of
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
for explanation of these arguments

It is not necessary to process all of those arguments, but they can be
used to modulate certain settings when calculating predictions. Note
that if your method does not define all mentioned arguments, these are
still passed via `...` - make sure that further methods in your
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
method still work when they process the `...`.

## Output: the data.frame structure

It is important that the function returns a data frame with a specific
structure, namely the data grid (argument `data_grid`, that was passed
to
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)),
including the additional columns `predicted`, `conf.low`, and
`conf.high` (which are the results of the
[`get_predictions()`](https://strengejacke.github.io/ggeffects/reference/get_predictions.md)
function). Note that predictions and intervals usually should be on the
response scale.

## Example

A simple example for an own class-implementation for Gaussian-alike
models could look like this:

``` r

get_predictions.own_class <- function(model, data_grid, ci_level = 0.95, ...) {
  predictions <- predict(
    model,
    newdata = data_grid,
    type = "response",
    se.fit = !is.na(ci_level),
    ...
  )

  # do we have standard errors?
  if (is.na(ci_level)) {
    # copy predictions
    data_grid$predicted <- as.vector(predictions)
  } else {
    # copy predictions
    data_grid$predicted <- predictions$fit

    # calculate CI
    data_grid$conf.low <- predictions$fit - qnorm(0.975) * predictions$se.fit
    data_grid$conf.high <- predictions$fit + qnorm(0.975) * predictions$se.fit

    # optional: copy standard errors
    attr(data_grid, "std.error") <- predictions$se.fit
  }

  data_grid
}
```

A simple example for an own class-implementation for non-Gaussian-alike
models could look like this (note the use of the link-inverse function
[`link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.html),
which is passed to the `link_inverse` argument):

``` r

get_predictions.own_class <- function(model,
                                      data_grid,
                                      ci_level = 0.95,
                                      link_inverse = insight::link_inverse(model),
                                      ...) {
  predictions <- predict(
    model,
    newdata = data_grid,
    type = "link", # for non-Gaussian, return on link-scale
    se.fit = !is.na(ci_level),
    ...
  )

  # do we have standard errors?
  if (is.na(ci_level)) {
    # copy predictions
    data_grid$predicted <- link_inverse(as.vector(predictions))
  } else {
    # copy predictions, use link-inverse to back-transform
    data_grid$predicted <- link_inverse(predictions$fit)

    # calculate CI
    data_grid$conf.low <- link_inverse(
      predictions$fit - qnorm(0.975) * predictions$se.fit
    )
    data_grid$conf.high <- link_inverse(
      predictions$fit + qnorm(0.975) * predictions$se.fit
    )

    # optional: copy standard errors
    attr(data_grid, "std.error") <- predictions$se.fit
  }

  data_grid
}
```
