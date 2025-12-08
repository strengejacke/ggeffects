# Calculate representative values of a vector

This function calculates representative values of a vector, like
minimum/maximum values or lower, median and upper quartile etc., which
can be used for numeric vectors to plot adjusted predictions at these
representative values.

## Usage

``` r
values_at(x, values = "meansd")

representative_values(x, values = "meansd")
```

## Arguments

- x:

  A numeric vector.

- values:

  Character vector, naming a pattern for which representative values
  should be calculcated.

  - `"minmax": `(default) minimum and maximum values (lower and upper
    bounds) of `x`.

  - `"meansd"`: uses the mean value of `x` as well as one standard
    deviation below and above mean value to plot the effect of the
    moderator on the independent variable.

  - `"zeromax"`: is similar to the `"minmax"` option, however, `0` is
    always used as minimum value for `x`. This may be useful for
    predictors that don't have an empirical zero-value, but absence of
    moderation should be simulated by using 0 as minimum.

  - `"fivenum"`: calculates and uses the Tukey's five number summary
    (minimum, lower-hinge, median, upper-hinge, maximum) of `x`. This is
    equivalent to `"quartiles"`.

  - `"threenum"`: calculates a three number summary (lower-hinge,
    median, and upper-hinge) of `x`. This is equivalent to
    `"quartiles2"`.

  - `"terciles"`: calculates and uses the terciles (lower and upper
    third) of `x`, *including* minimum and maximum value.

  - `"terciles2"`: calculates and uses the terciles (lower and upper
    third) of `x`, *excluding* minimum and maximum value.

  - an option to compute a range of percentiles is also possible, using
    `"percentile"`, followed by the percentage of the range. For
    example, `"percentile95"` will calculate the 95% range of `x`.

  - `"all"`: uses all values of `x`.

## Value

A numeric vector, representing the required values from `x`, like
minimum/maximum value or mean and +/- 1 SD. If `x` is missing, a
function, pre-programmed with `n` and `length` is returned. See
examples.

## Examples

``` r
data(efc)
values_at(efc$c12hour)
#> [1] -8.4 42.4 93.2
values_at(efc$c12hour, "quartiles2")
#> [1] 10.0 20.0 42.8

mean_sd <- values_at(values = "meansd")
mean_sd(efc$c12hour)
#> [1] -8.4 42.4 93.2
```
