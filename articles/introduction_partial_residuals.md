# Introduction: Adding Partial Residuals to Adjusted Predictions Plots

Plotting partial residuals on top of the estimated marginal means allows
detecting missed modeling, like unmodeled non-linear relationships or
unmodeled interactions. In a nutshell, it allows *Visualizing Fit and
Lack of Fit in Complex Regression Models with Predictor Effect Plots and
Partial Residuals* (Fox & Weisberg 2018).

To add partial residuals to a plot, add `show_residuals = TRUE` to the
[`plot()`](https://strengejacke.github.io/ggeffects/reference/plot.md)
function call. Unlike plotting raw data, partial residuals are much
better in detecting spurious patterns of relationships between
predictors and outcome.

### Detecting non-linear relationship

Let’s look at an example with a non-linear relationship. The missed
pattern is not obvious when looking at the raw data:

[`library`](https://rdrr.io/r/base/library.html)`(`[`ggeffects`](https://strengejacke.github.io/ggeffects/)`)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``x`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``200``)`` ``z`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``200``)`` ``# quadratic relationship`` ``y`` ``<-`` ``2`` ``*`` ``x`` ``+`` ``x``^``2`` ``+`` ``4`` ``*`` ``z`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``200``)`` `` ``d`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``x``, ``y``, ``z``)`` ``m`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``y`` ``~`` ``x`` ``+`` ``z``, data ``=`` ``d``)`` `` ``pr`` ``<-`` `[`predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)`(``m``, ``"x [all]"``)`` `[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_data ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-1-1.png)

However, it becomes more obvious with partial residuals:

[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_residuals ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-2-1.png)

It is even more obvious, when a local polynomial regression line (loess)
is added to the plot. This can be achieved using
`show_residuals_line = TRUE`.

[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_residuals ``=`` ``TRUE``, show_residuals_line ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-3-1.png)

### Detecting missed interactions

Here is another example, which shows that the partial residuals plot
suggests modeling an interaction:

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``x`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``, mean ``=`` ``10``)`` ``z`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``)`` ``v`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``)`` ``y`` ``<-`` ``(``4`` ``*`` ``z`` ``+`` ``2``)`` ``*`` ``x`` ``-`` ``40`` ``*`` ``z`` ``+`` ``5`` ``*`` ``v`` ``+`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``300``, sd ``=`` ``3``)`` `` ``d`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``x``, ``y``, ``z``)`` ``m`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``y`` ``~`` ``x`` ``+`` ``z``, data ``=`` ``d``)`` `` ``pr`` ``<-`` `[`predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)`(``m``, `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"z"``)``)`` `` ``# raw data, no interaction`` `[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_data ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-4-1.png)

Again, it is recommended to add a loess-fit line to the residuals:

[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_residuals ``=`` ``TRUE``, grid ``=`` ``TRUE``, show_residuals_line ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-5-1.png)

Modeling the interaction clearly catches the pattern in the data better.

`m`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``y`` ``~`` ``x`` ``*`` ``z``, data ``=`` ``d``)`` ``pr`` ``<-`` `[`predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)`(``m``, `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"z"``)``)`` `[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_residuals ``=`` ``TRUE``, grid ``=`` ``TRUE``, show_residuals_line ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-6-1.png)

### Using the complete range of values

*ggeffects* usually “prettyfies” the data and tries to find a pretty
sequence over a range of a focal predictor, to avoid too lengthy output,
particularly for continuous variables (see section *pretty value ranges*
in [this
vignette](https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.md)).

This, however, might be misleading in some cases when creating residual
plots. In the next example, we have a sinus-curve pattern for the
residuals, which is hidden by default:

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``x`` ``<-`` `[`seq`](https://rdrr.io/r/base/seq.html)`(``-``100``, ``100``, length.out ``=`` ``1e3``)`` ``z`` ``<-`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``1e3``)`` ``y`` ``<-`` ``5`` ``*`` `[`sin`](https://rdrr.io/r/base/Trig.html)`(``x`` ``/`` ``2``)`` ``+`` ``x`` ``/`` ``2`` ``+`` ``10`` ``*`` ``z`` `` ``m`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``y`` ``~`` ``x`` ``+`` ``z``)`` ``pr`` ``<-`` `[`predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)`(``m``, ``"x"``)`` `` `[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_residuals ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-7-1.png)

In such cases, it is recommended to use the `all`-tag in the
`terms`-argument.

`pr`` ``<-`` `[`predict_response`](https://strengejacke.github.io/ggeffects/reference/predict_response.md)`(``m``, ``"x [all]"``)`` `[`plot`](https://strengejacke.github.io/ggeffects/reference/plot.md)`(``pr``, show_residuals ``=`` ``TRUE``)`

![](introduction_partial_residuals_files/figure-html/unnamed-chunk-8-1.png)

## References

Fox J, Weisberg S. *Visualizing Fit and Lack of Fit in Complex
Regression Models with Predictor Effect Plots and Partial Residuals*.
Journal of Statistical Software 2018;87.
<https://www.jstatsoft.org/article/view/v087i09>
