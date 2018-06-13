# ggeffects - Create Tidy Data Frames of Marginal Effects for 'ggplot' from Model Outputs <img src="man/figures/logo.png" align="right" />

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1249195.svg)](https://doi.org/10.5281/zenodo.1249195)

## Benefits from calculating marginal effects

Results of regression models are typically presented as tables that are easy to understand. For more complex models that include interaction or quadratic / spline terms, tables with numbers are less helpful and difficult to interpret. In such cases, _marginal effects_ are far easier to understand. In particular, the visualization of marginal effects makes it possible to intuitively get the idea of how predictors and outcome are associated, even for complex models. 

## ggeffects supports many different models and is easy to apply

This package computes marginal effects at the mean or average marginal effects from statistical models and returns the result as tidy data frames. These data frames are ready to use with the _ggplot2_-package. Marginal effects can be calculated for many different models. Currently supported model-objects are: `lm`, `glm`, `glm.nb`, `lme`, `lmer`, `glmer`, `glmer.nb`, `nlmer`, `glmmTMB`, `gam` (package *mgcv*), `vgam`, `gamm`, `gamm4`, `multinom`, `betareg`, `truncreg`, `coxph`, `gls`, `gee`, `plm`, `lrm`, `polr`, `clm`, `zeroinfl`, `hurdle`, `stanreg`, `brmsfit`, `svyglm` and `svyglm.nb`. Other models not listed here are passed to a generic predict-function and might work as well, or maybe with `ggeffect()`, which effectively does the same as `ggpredict()`.

Interaction terms, splines and polynomial terms are also supported. The two main functions are `ggpredict()` and `ggaverage()`, however, there are some convenient wrapper-functions especially for polynomials or interactions. There is a generic `plot()`-method to plot the results using _ggplot2_.

## Examples

The returned data frames always have the same, consistent structure and column names, so it's easy to create ggplot-plots without the need to re-write the function call. `x` and `predicted` are the values for the x- and y-axis. `conf.low` and `conf.high` could be used as `ymin` and `ymax` aesthetics for ribbons to add confidence bands to the plot. `group` can be used as grouping-aesthetics, or for faceting.

`ggpredict()` requires at least one, but not more than three terms specified in the `terms`-argument. Predicted values of the response, along the values of the first term are calucalted, optionally grouped by the other terms specified in `terms`.

```
data(efc)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

ggpredict(fit, terms = "c12hour")
#> # A tibble: 62 × 6
#>        x predicted conf.low conf.high  group
#>    <dbl>     <dbl>    <dbl>     <dbl> <fctr>
#> 1      4  74.43040 72.33073  76.53006      1
#> 2      5  74.17710 72.09831  76.25588      1
#> 3      6  73.92379 71.86555  75.98204      1
#> 4      7  73.67049 71.63242  75.70857      1
#> 5      8  73.41719 71.39892  75.43546      1
#> 6      9  73.16389 71.16504  75.16275      1
#> 7     10  72.91059 70.93076  74.89042      1
#> 8     11  72.65729 70.69608  74.61850      1
#> 9     12  72.40399 70.46098  74.34700      1
#> 10    14  71.89738 69.98948  73.80529      1
#> # ... with 52 more rows
```

A possible call to ggplot could look like this:

```
library(ggplot2)
mydf <- ggpredict(fit, terms = "c12hour")
ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
```

However, there is also a `plot()`-method. This method uses convenient defaults, to easily create the most suitable plot for the marginal effects.

```
mydf <- ggpredict(fit, terms = "c12hour")
plot(mydf)
```

`plot()` offers a few, but useful arguments, so it's easy to use.

With three variables, predictions can be grouped and faceted.

```
ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
#> # A tibble: 372 × 7
#>        x predicted  conf.low conf.high                           group      facet
#>    <dbl>     <dbl>     <dbl>     <dbl>                          <fctr>     <fctr>
#> 1      4  74.70073  72.38031  77.02114 intermediate level of education [2] Female
#> 2      4  73.98237  70.45711  77.50763          low level of education [2] Female
#> 3      4  75.41908  71.91747  78.92070         high level of education [2] Female
#> 4      4  73.65930  70.08827  77.23033 intermediate level of education   [1] Male
#> 5      4  72.94094  68.38540  77.49649          low level of education   [1] Male
#> 6      4  74.37766  70.05658  78.69874         high level of education   [1] Male
#> 7      5  74.44742  72.14644  76.74841 intermediate level of education [2] Female
#> 8      5  73.72907  70.21926  77.23888          low level of education [2] Female
#> 9      5  75.16578  71.67430  78.65726         high level of education [2] Female
#> 10     5  73.40600  69.84575  76.96625 intermediate level of education   [1] Male
#> # ... with 362 more rows

mydf <- ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet)
```

`plot()` works for this case, as well.

There are some more features, which are explained in more detail in the package-vignette.

## Contributing to the package

Please follow [this guide](CONTRIBUNTING.md) if you like to contribute to this package.

## Documentation

Please visit [https://strengejacke.github.io/ggeffects/](https://strengejacke.github.io/ggeffects/) for documentation and vignettes.

## Installation

### Latest development build

To install the latest development snapshot (see latest changes below), type following commands into the R console:

```r
library(devtools)
devtools::install_github("strengejacke/ggeffects")
```

Please note the package dependencies when installing from GitHub. The GitHub version of this package may depend on latest GitHub versions of my other packages, so you may need to install those first, if you encounter any problems. Here's the order for installing packages from GitHub:

[sjlabelled](https://github.com/strengejacke/sjlabelled) &rarr; [sjmisc](https://github.com/strengejacke/sjmisc) &rarr; [sjstats](https://github.com/strengejacke/sjstats) &rarr; [ggeffects](https://github.com/strengejacke/ggeffects) &rarr; [sjPlot](https://github.com/strengejacke/sjPlot)


### Officiale, stable release

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggeffects)](https://cran.r-project.org/package=ggeffects)
&#160;&#160;
[![downloads](http://cranlogs.r-pkg.org/badges/ggeffects)](http://cranlogs.r-pkg.org/)
&#160;&#160;
[![total](http://cranlogs.r-pkg.org/badges/grand-total/ggeffects)](http://cranlogs.r-pkg.org/)

To install the latest stable release from CRAN, type following command into the R console:

```r
install.packages("ggeffects")
```

## Citation

In case you want / have to cite my package, please use `citation('ggeffects')` for citation information.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1249195.svg)](https://doi.org/10.5281/zenodo.1249195)
