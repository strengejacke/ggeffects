# S3-class definition for the ggeffects package

`get_predictions()` is the core function to return adjusted predictions
for a model, when calling
[`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
or
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
with `margin = "mean_reference"` (the default option for `margin`).
Basically, the input contains the model object and a data grid that is
typically used for the `newdata` argument of the
[`predict()`](https://rdrr.io/r/stats/predict.html) method.
`get_predictions()` can be used as S3-method for own classes, to add
support for new models in **ggeffects** and is only relevant for package
developers.

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

## Usage

``` r
get_predictions(model, ...)

# Default S3 method
get_predictions(
  model,
  data_grid = NULL,
  terms = NULL,
  ci_level = 0.95,
  type = NULL,
  typical = NULL,
  vcov = NULL,
  vcov_args = NULL,
  condition = NULL,
  interval = "confidence",
  bias_correction = FALSE,
  link_inverse = insight::link_inverse(model),
  model_info = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model, terms, ci_level, type, typical, vcov, vcov_args, condition,
  interval, bias_correction, verbose:

  Arguments from the call to
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  that are passed down to `get_predictions()`. Note that
  `bias_correction` is usally already processed in
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  and thus doesn't need further handling in `get_predictions()`, unless
  you need to re-calculate the link-inverse-function (argument
  `link_inverse`) inside the `get_predictions()` method.

- ...:

  Further arguments, passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html) or other methods
  used in `get_predictions()`.

- data_grid:

  A data frame containing the data grid (or reference grid) with all
  relevant values of predictors for which the adjusted predictions
  should be made. Typically the data frame that is passed to the
  `newdata` argument in
  [`predict()`](https://rdrr.io/r/stats/predict.html). A data grid can
  be created with functions like
  [`data_grid()`](https://strengejacke.github.io/ggeffects/reference/new_data.md)
  or
  [`insight::get_datagrid()`](https://easystats.github.io/insight/reference/get_datagrid.html).

- link_inverse:

  The model's family link-inverse function. Can be retrieved using
  [`insight::link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.html).

- model_info:

  An object returned by
  [`insight::model_info()`](https://easystats.github.io/insight/reference/model_info.html).

## Value

A data frame that contains

- the data grid (from the argument `data_grid`)

- the columns `predicted`, `conf.low`, and `conf.high`

- optionally, the attribute `"std.error"` with the standard errors.

Note that predictions and confidence intervals should already be
transformed to the *response* scale (e.g., by using
[`insight::link_inverse()`](https://easystats.github.io/insight/reference/link_inverse.html)).
The *standard errors* are always on the link scale (not transformed).

If values are not available (for example, confidence intervals), set
their value to `NA`.

## Details

Adding support for **ggeffects** is quite easy. The user-level function
is
[`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md),
which either calls
[`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
[`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md)
or
[`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
These function, in turn, call
[`predict()`](https://rdrr.io/r/stats/predict.html),
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
or
[`marginaleffects::avg_predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html).
Following needs to be done to add support for new model classes:

- **emmeans**: if your model is supported by emmeans, it is
  automatically supported by
  [`ggemmeans()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).
  Thus, you need to add the corresponding methods to your package so
  that your model class is supported by \*\*emmeans.

- **marginaleffects**: similar to **emmeans**, if your package is
  supported by the **marginaleffects** package, it works with
  [`ggaverage()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md).

- **predict**: in order to make your model class work with
  [`ggpredict()`](https://strengejacke.github.io/ggeffects/reference/ggpredict.md),
  you need to add a `get_predictions()` method. The here documented
  arguments are *all* passed from
  [`predict_response()`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)
  to `get_predictions()`, no matter if they are required to calculate
  predictions or not. Thus, it is not necessary to process all of those
  arguments, but they can be used to modulate certain settings when
  calculating predictions. Note that if your method does not define all
  mentioned arguments, these are still passed via `...` - make sure that
  further methods in your `get_predictions()` method still work when
  they process the `...`. It is important that the function returns a
  data frame with a specific structure, namely the data grid and the
  columns `predicted`, `conf.low`, and `conf.high`. Predictions and
  intervals should be on the response scale.

A simple example for an own class-implementation for Gaussian-alike
models could look like this:

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

A simple example for an own class-implementation for non-Gaussian-alike
models could look like this (note the use of the link-inverse function
`link_inverse()`, which is passed to the `link_inverse` argument):

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
