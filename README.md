# ggeffects - Create Tidy Data Frames of Marginal Effects for 'ggplot' from Model Outputs <img src="man/figures/logo.png" align="right" />

[![DOI](http://joss.theoj.org/papers/10.21105/joss.00772/status.svg)](https://doi.org/10.21105/joss.00772)

## Why do we need marginal effects?

Results of regression models are typically presented as tables that are easy to understand. For more complex models that include interaction or quadratic / spline terms, tables with numbers are less helpful and difficult to interpret. In such cases, _marginal effects_ are far easier to understand. In particular, the visualization of marginal effects makes it possible to intuitively get the idea of how predictors and outcome are associated, even for complex models. 

## Aim of this package

**ggeffects** computes marginal effects at the mean or average marginal effects from statistical models and returns the result as tidy data frames. These data frames are ready to use with the **ggplot2**-package.

## Documentation and Support

Please visit [https://strengejacke.github.io/ggeffects/](https://strengejacke.github.io/ggeffects/) for documentation and vignettes. In case you want to file an issue or contribute in another way to the package, please follow [this guide](CONTRIBUTING.md). For questions about the functionality, you may either contact me via email or also file an issue.

## ggeffects supports many different models and is easy to use

Marginal effects can be calculated for many different models. Currently supported model-objects are: `lm`, `glm`, `glm.nb`, `lme`, `lmer`, `glmer`, `glmer.nb`, `nlmer`, `glmmTMB`, `gam` (package **mgcv**), `vgam`, `gamm`, `gamm4`, `multinom`, `betareg`, `truncreg`, `coxph`, `gls`, `gee`, `plm`, `lrm`, `polr`, `clm`, `zeroinfl`, `hurdle`, `stanreg`, `brmsfit`, `lmRob`, `glmRob`, `brglm`, `rlm`, `svyglm` and `svyglm.nb`. Other models not listed here are passed to a generic predict-function and might work as well, or maybe with `ggeffect()`, which effectively does the same as `ggpredict()`.

Interaction terms, splines and polynomial terms are also supported. The two main functions are `ggpredict()` and `ggaverage()`, however, there are some convenient wrapper-functions especially for polynomials or interactions. There is a generic `plot()`-method to plot the results using **ggplot2**.

## Examples

The returned data frames always have the same, consistent structure and column names, so it's easy to create ggplot-plots without the need to re-write the function call. `x` and `predicted` are the values for the x- and y-axis. `conf.low` and `conf.high` could be used as `ymin` and `ymax` aesthetics for ribbons to add confidence bands to the plot. `group` can be used as grouping-aesthetics, or for faceting.

`ggpredict()` requires at least one, but not more than three terms specified in the `terms`-argument. Predicted values of the response, along the values of the first term are calucalted, optionally grouped by the other terms specified in `terms`.

```
data(efc)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

ggpredict(fit, terms = "c12hour")
#> # A tibble: 35 x 5
#>        x predicted conf.low conf.high group
#>    <dbl>     <dbl>    <dbl>     <dbl> <fct>
#>  1     0      75.4     73.3      77.6 1    
#>  2     5      74.2     72.1      76.3 1    
#>  3    10      72.9     70.9      74.9 1    
#>  4    15      71.6     69.8      73.5 1    
#>  5    20      70.4     68.6      72.2 1    
#>  6    25      69.1     67.4      70.9 1    
#>  7    30      67.8     66.1      69.5 1    
#>  8    35      66.6     64.9      68.2 1    
#>  9    40      65.3     63.7      67.0 1    
#> 10    45      64.0     62.4      65.7 1    
#> # ... with 25 more rows
```

A possible call to ggplot could look like this:

```
library(ggplot2)
mydf <- ggpredict(fit, terms = "c12hour")
ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
```
![](man/figures/README-example-1.png)

However, there is also a `plot()`-method. This method uses convenient defaults, to easily create the most suitable plot for the marginal effects.

```
mydf <- ggpredict(fit, terms = "c12hour")
plot(mydf)
```
![](man/figures/README-example-2.png)

`plot()` offers a few, but useful arguments, so it's easy to use.

With three variables, predictions can be grouped and faceted.

```
ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
#> # A tibble: 210 x 6
#>        x predicted conf.low conf.high group                           facet     
#>    <dbl>     <dbl>    <dbl>     <dbl> <fct>                           <fct>     
#>  1     0      75.0     71.4      78.6 low level of education          [2] Female
#>  2     0      74.0     69.4      78.6 low level of education          [1] Male  
#>  3     0      75.7     73.3      78.1 intermediate level of education [2] Female
#>  4     0      74.7     71.1      78.3 intermediate level of education [1] Male  
#>  5     0      76.4     72.9      80.0 high level of education         [2] Female
#>  6     0      75.4     71.0      79.7 high level of education         [1] Male  
#>  7     5      73.7     70.2      77.2 low level of education          [2] Female
#>  8     5      72.7     68.1      77.2 low level of education          [1] Male  
#>  9     5      74.4     72.1      76.7 intermediate level of education [2] Female
#> 10     5      73.4     69.8      77.0 intermediate level of education [1] Male  
#> # ... with 200 more rows

mydf <- ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet)
```
![](man/figures/README-example-3.png)

`plot()` works for this case, as well:

```
plot(mydf)
```
![](man/figures/README-example-4.png)

There are some more features, which are explained in more detail in the package-vignette.

## Contributing to the package

Please follow [this guide](CONTRIBUTING.md) if you like to contribute to this package.

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

[![DOI](http://joss.theoj.org/papers/10.21105/joss.00772/status.svg)](https://doi.org/10.21105/joss.00772)
