% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_frame_methods.R, R/ggpredict.R,
%   R/predict_response.R
\name{as.data.frame.ggeffects}
\alias{as.data.frame.ggeffects}
\alias{ggpredict}
\alias{predict_response}
\title{Marginal effects, adjusted predictions and estimated marginal means from regression models}
\usage{
\method{as.data.frame}{ggeffects}(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  terms_to_colnames = FALSE
)

ggpredict(
  model,
  terms,
  ci_level = 0.95,
  type = "fixed",
  typical = "mean",
  condition = NULL,
  back_transform = TRUE,
  ppd = FALSE,
  vcov_fun = NULL,
  vcov_type = NULL,
  vcov_args = NULL,
  interval,
  verbose = TRUE,
  ci.lvl = ci_level,
  back.transform = back_transform,
  vcov.fun = vcov_fun,
  vcov.type = vcov_type,
  vcov.args = vcov_args,
  ...
)

predict_response(
  model,
  terms,
  marginalize = "mean_reference",
  ci_level = 0.95,
  type = "fixed",
  condition = NULL,
  back_transform = TRUE,
  ppd = FALSE,
  vcov_fun = NULL,
  vcov_type = NULL,
  vcov_args = NULL,
  interval,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x}{An object of class \code{ggeffects}, as returned by \code{predict_response()},
\code{ggpredict()}, \code{ggeffect()}, \code{ggaverage()} or \code{ggemmeans()}.}

\item{row.names}{\code{NULL} or a character vector giving the row
    names for the data frame.  Missing values are not allowed.}

\item{optional}{logical. If \code{TRUE}, setting row names and
    converting column names (to syntactic names: see
    \code{\link[base]{make.names}}) is optional.  Note that all of \R's
    \pkg{base} package \code{as.data.frame()} methods use
    \code{optional} only for column names treatment, basically with the
    meaning of \code{\link[base]{data.frame}(*, check.names = !optional)}.
    See also the \code{make.names} argument of the \code{matrix} method.}

\item{...}{For \code{ggpredict()}, further arguments passed down to \code{predict()};
for \code{ggeffect()}, further arguments passed down to \code{effects::Effect()}; for
\code{ggemmeans()}, further arguments passed down to \code{emmeans::emmeans()}; and
for \code{ggaverage()}, further arguments passed down to
\code{marginaleffects::avg_predictions()}.  If \code{type = "simulate"}, \code{...} may
also be used to set the number of simulation, e.g. \code{nsim = 500}.}

\item{stringsAsFactors}{logical: should the character vector be converted
    to a factor?}

\item{terms_to_colnames}{Logical, if \code{TRUE}, standardized column names (like
\code{"x"}, \code{"group"} or \code{"facet"}) are replaced by the variable names of the focal
predictors specified in \code{terms}.}

\item{model}{A model object, or a list of model objects.}

\item{terms}{Names of those terms from \code{model}, for which predictions should
be displayed (so called \emph{focal terms}). Can be:
\itemize{
\item A character vector, specifying the names of the focal terms. This is the
preferred and probably most flexible way to specify focal terms, e.g.
\code{terms = "x [40:60]"}, to calculate predictions for the values 40 to 60.
\item A list, where each element is a named vector, specifying the focal terms
and their values. This is the "classical" R way to specify focal terms,
e.g. \code{list(x = 40:60)}.
\item A formula, e.g. \code{terms = ~ x + z}, which is internally converted to a
character vector. This is probably the least flexible way, as you cannot
specify representative values for the focal terms.
\item A data frame representig a "data grid" or "reference grid". Predictions
are then made for all combinations of the variables in the data frame.
}

At least one term is required to calculate effects for certain terms,
maximum length is four terms, where the second to fourth term indicate the
groups, i.e. predictions of first term are grouped at meaningful values or
levels of the remaining terms (see \code{\link[=values_at]{values_at()}}). If \code{terms} is missing
or \code{NULL}, adjusted predictions for each model term are calculated (i.e.
each model term is used as single focal term). It is also possible to define
specific values for focal terms, at which adjusted predictions should be
calculated (see 'Details'). All remaining covariates that are not specified
in \code{terms} are held constant (see 'Details'). See also arguments \code{condition}
and \code{typical}.}

\item{ci_level}{Numeric, the level of the confidence intervals. For \code{ggpredict()},
use \code{ci_level = NA}, if confidence intervals should not be calculated
(for instance, due to computation time). Typically, confidence intervals
based on the standard errors as returned by the \code{predict()} function
are returned, assuming a t- or normal distribution (based on the model and
the available degrees of freedom, i.e. roughly \verb{+/- 1.96 * SE}). See introduction
of \href{https://strengejacke.github.io/ggeffects/articles/ggeffects.html}{this vignette}
for more details.}

\item{type}{Character, indicating whether predictions should be conditioned
on specific model components or not. Consequently, most options only apply
for survival models, mixed effects models and/or models with zero-inflation
(and their Bayesian counter-parts); only exeption is \code{type = "simulate"},
which is available for some other model classes as well (which respond to
\code{simulate()}).

\strong{Note 1:} For \code{brmsfit}-models with zero-inflation component,
there is no \code{type = "zero_inflated"} nor \code{type = "zi_random"}; predicted
values for \code{MixMod}-models from \strong{GLMMadaptive} with zero-inflation
component \emph{always} condition on the zero-inflation part of the model (see
'Details').

\strong{Note 2:} If \verb{marginalize = "empirical")} the \code{type} argument is handled
differently. It is set to \code{"response"} by default, and usually accepts all
values by the \code{type}-argument of the model's respetive \code{predict()}-method.
E.g., passing a \code{glm} object would allow the options \code{"response"}, \code{"link"},
and \code{"terms"}. Thus, the following options apply to \code{predict_response()} when
\code{marginalize} is not \code{"empirical"}:
\itemize{
\item \code{"fixed"} (or \code{"fe"} or \code{"count"})

Predicted values are conditioned on the fixed effects or conditional
model only (for mixed models: predicted values are on the population-level
and \emph{confidence intervals} are returned, i.e. \code{re.form = NA} when calling
\code{predict()}). For instance, for models fitted with \code{zeroinfl} from \strong{pscl},
this would return the predicted mean from the count component (without
zero-inflation). For models with zero-inflation component, this type calls
\code{predict(..., type = "link")} (however, predicted values are
back-transformed to the response scale).
\item \code{"fixed_ppd"}

Only applies to \code{marginalize = "mean_reference"}, and only for Bayesian
models of class \code{stanreg} or \code{brmsfit}. Computes the posterior predictive
distribution. It is the same as setting \code{type = "fixed"} in combination with
\code{ppd = TRUE}.
\item \code{"random"} (or \code{"re"})

This only applies to mixed models, and \code{type = "random"} does not condition
on the zero-inflation component of the model. \code{type = "random"} still
returns population-level predictions, however, conditioned on random effects
and considering individual level predictions, i.e. \code{re.form = NULL} when
calling \code{predict()}. This may affect the returned predicted values, depending
on whether \code{REML = TRUE} or \code{REML = FALSE} was used for model fitting.
Furthermore, unlike \code{type = "fixed"}, intervals also consider the uncertainty
in the variance parameters (the mean random effect variance, see \emph{Johnson
et al. 2014} for details) and hence can be considered as \emph{prediction intervals}.
For models with zero-inflation component, this type calls
\code{predict(..., type = "link")} (however, predicted values are back-transformed
to the response scale).

To get predicted values for each level of the random effects groups, add the
name of the related random effect term to the \code{terms}-argument
(for more details, see
\href{https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html}{this vignette}).
\item \code{"random_ppd"}

Only applies to \code{marginalize = "mean_reference"},, and only for Bayesian
models of class \code{stanreg} or \code{brmsfit}. Computes the posterior predictive
distribution. It is the same as setting \code{type = "random"} in combination with
\code{ppd = TRUE}.
\item \code{"zero_inflated"} (or \code{"fe.zi"} or \code{"zi"})

Predicted values are conditioned on the fixed effects and the zero-inflation
component. For instance, for models fitted with \code{zeroinfl}
from \strong{pscl}, this would return the predicted response (\code{mu*(1-p)})
and for \strong{glmmTMB}, this would return the expected value \code{mu*(1-p)}
\emph{without} conditioning on random effects (i.e. random effect variances
are not taken into account for the confidence intervals). For models with
zero-inflation component, this type calls \code{predict(..., type = "response")}.
See 'Details'.
\item \code{"zi_random"} (or \code{"re.zi"} or \code{"zero_inflated_random"})

Predicted values are conditioned on the zero-inflation component and
take the random effects uncertainty into account. For models fitted with
\code{glmmTMB()}, \code{hurdle()} or \code{zeroinfl()}, this would return the
expected value \code{mu*(1-p)}. For \strong{glmmTMB}, prediction intervals
also consider the uncertainty in the random effects variances. This
type calls \code{predict(..., type = "response")}. See 'Details'.
\item \code{"zi_prob"} (or \code{"zi.prob"})

Predicted zero-inflation probability. For \strong{glmmTMB} models with
zero-inflation component, this type calls \code{predict(..., type = "zlink")};
models from \strong{pscl} call \code{predict(..., type = "zero")} and for
\strong{GLMMadaptive}, \code{predict(..., type = "zero_part")} is called.
\item \code{"simulate"} (or \code{"sim"})

Predicted values and confidence resp. prediction intervals are
based on simulations, i.e. calls to \code{simulate()}. This type
of prediction takes all model uncertainty into account, including
random effects variances. Currently supported models are objects of
class \code{lm}, \code{glm}, \code{glmmTMB}, \code{wbm}, \code{MixMod}
and \code{merMod}. See \code{...} for details on number of simulations.
\item \code{"survival"} and \code{"cumulative_hazard"} (or \code{"surv"} and \code{"cumhaz"})

Applies only to \code{coxph}-objects from the \strong{survial}-package and
calculates the survival probability or the cumulative hazard of an event.
}}

\item{typical}{Character vector, naming the function to be applied to the
covariates (non-focal terms) over which the effect is "averaged". The
default is \code{"mean"}. Can be \code{"mean"}, "\code{weighted.mean}", \code{"median"}, \code{"mode"}
or \code{"zero"}, which call the corresponding R functions (except \code{"mode"},
which calls an internal function to compute the most common value); \code{"zero"}
simply returns 0. By default, if the covariate is a factor, only \code{"mode"} is
applicable; for all other values (including the default, \code{"mean"}) the
reference level is returned. For character vectors, only the mode is returned.
You can use a named vector to apply different functions to integer, numeric and
categorical covariates, e.g. \code{typical = c(numeric = "median", factor = "mode")}.
If \code{typical} is \code{"weighted.mean"}, weights from the model are used. If no
weights are available, the function falls back to \code{"mean"}. \strong{Note} that this
argument is ignored for \code{predict_response()}, because the \code{marginalize} argument
takes care of this.}

\item{condition}{Named character vector, which indicates covariates that
should be held constant at specific values. Unlike \code{typical}, which
applies a function to the covariates to determine the value that is used
to hold these covariates constant, \code{condition} can be used to define
exact values, for instance \code{condition = c(covariate1 = 20, covariate2 = 5)}.
See 'Examples'.}

\item{back_transform}{Logical, if \code{TRUE} (the default), predicted values
for log- or log-log transformed responses will be back-transformed to
original response-scale.}

\item{ppd}{Logical, if \code{TRUE}, predictions for Stan-models are based on the
posterior predictive distribution \code{\link[rstantools:posterior_predict]{rstantools::posterior_predict()}}. If
\code{FALSE} (the default), predictions are based on posterior draws of the linear
predictor \code{\link[rstantools:posterior_linpred]{rstantools::posterior_linpred()}}.}

\item{vcov_fun}{Variance-covariance matrix used to compute uncertainty
estimates (e.g., for confidence intervals based on robust standard errors).
This argument accepts a covariance matrix, a function which returns a
covariance matrix, or a string which identifies the function to be used to
compute the covariance matrix.
\itemize{
\item A (variance-covariance) matrix
\item A function which returns a covariance matrix (e.g., \code{stats::vcov()})
\item A string which indicates the estimation type for the heteroscedasticity-consistent
variance-covariance matrix, e.g. \code{vcov_fun = "HC0"}. Possible values are
\code{"HC0"}, \code{"HC1"}, \code{"HC2"}, \code{"HC3"}, \code{"HC4"}, \code{"HC4m"}, and \code{"HC5"}, which
will then call the \code{vcovHC()}-function from the \strong{sandwich} package, using
the specified type. Further possible values are \code{"CR0"}, \code{"CR1"}, \code{"CR1p"},
\code{"CR1S"}, \code{"CR2"}, and \code{"CR3"}, which will call the \code{vcovCR()}-function from
the \strong{clubSandwich} package.
\item A string which indicates the name of the \verb{vcov*()}-function from the
\strong{sandwich} or \strong{clubSandwich} packages, e.g. \code{vcov_fun = "vcovCL"},
which is used to compute (cluster) robust standard errors for predictions.
}

If \code{NULL}, standard errors (and confidence intervals) for predictions are
based on the standard errors as returned by the \code{predict()}-function.
\strong{Note} that probably not all model objects that work with \code{ggpredict()}
are also supported by the \strong{sandwich} or \strong{clubSandwich} packages.

See details in \href{https://strengejacke.github.io/ggeffects/articles/practical_robustestimation.html}{this vignette}.}

\item{vcov_type}{Character vector, specifying the estimation type for the
robust covariance matrix estimation (see \code{?sandwich::vcovHC}
or \code{?clubSandwich::vcovCR} for details). Only used when \code{vcov_fun} is a
character string indicating one of the functions from those packages.}

\item{vcov_args}{List of named vectors, used as additional arguments that
are passed down to \code{vcov_fun}.}

\item{interval}{Type of interval calculation, can either be \code{"confidence"}
(default) or \code{"prediction"}. May be abbreviated. Unlike \emph{confidence intervals},
\emph{prediction intervals} include the residual variance (sigma^2) to account for
the uncertainty of predicted values. For mixed models, \code{interval = "prediction"}
is the default for \code{type = "random"}. When \code{type = "fixed"}, the default is
\code{interval = "confidence"}. Note that prediction intervals are not available
for all models, but only for models that work with \code{\link[insight:get_sigma]{insight::get_sigma()}}.}

\item{verbose}{Toggle messages or warnings.}

\item{ci.lvl, vcov.fun, vcov.type, vcov.args, back.transform}{Deprecated arguments.
Please use \code{ci_level}, \code{vcov_fun}, \code{vcov_type}, \code{vcov_args} and \code{back_transform}
instead.}

\item{marginalize}{Character string, indicating how to marginalize over the
\emph{non-focal} predictors, i.e. those variables that are \emph{not} specified in
\code{terms}. Possible values are \code{"mean_reference"}, \code{"mean_mode"} (both aka
"conditional effects"), \code{"marginalmeans"} (aka "marginal effects") and
\code{"empirical"} (or one of its aliases, \code{"counterfactual"} or \code{"ame"}, aka
average marginal effects). You can set a default-option for the \code{marginalize}
argument via \code{options()}, e.g. \code{options(ggeffects_marginalize = "empirical")},
so you don't have to specify your preferred marginalization method each time
you call \code{predict_response()}. \strong{Note}: \code{marginalize} replaces the \code{typical}
argument. E.g. if you set \code{marginalize = "mean_mode"}, you don't have to specify
\code{typical = c(numeric = "mean", factor = "mode")} anymore. Other available
marginalization options cannot be achieved via \code{typical}, thus \code{marginalize}
(and therefore, \code{predict_response()}) is the preferred way to specify the
marginalization method.}
}
\value{
A data frame (with \code{ggeffects} class attribute) with consistent data columns:
\itemize{
\item \code{"x"}: the values of the first term in \code{terms}, used as x-position in plots.
\item \code{"predicted"}: the predicted values of the response, used as y-position in plots.
\item \code{"std.error"}: the standard error of the predictions. \emph{Note that the standard
errors are always on the link-scale, and not back-transformed for non-Gaussian
models!}
\item \code{"conf.low"}: the lower bound of the confidence interval for the predicted values.
\item \code{"conf.high"}: the upper bound of the confidence interval for the predicted values.
\item \code{"group"}: the grouping level from the second term in \code{terms}, used as
grouping-aesthetics in plots.
\item \code{"facet"}: the grouping level from the third term in \code{terms}, used to indicate
facets in plots.

The estimated marginal means (or predicted values) are always on the
response scale!

For proportional odds logistic regression (see \code{?MASS::polr})
resp. cumulative link models (e.g., see \code{?ordinal::clm}),
an additional column \code{"response.level"} is returned, which indicates
the grouping of predictions based on the level of the model's response.

Note that for convenience reasons, the columns for the intervals
are always named \code{"conf.low"} and \code{"conf.high"}, even though
for Bayesian models credible or highest posterior density intervals
are returned.

There is an \code{\link[=as.data.frame]{as.data.frame()}} method for objects of class \code{ggeffects},
which has an \code{terms_to_colnames} argument, to use the term names as column
names instead of the standardized names \code{"x"} etc.
}
}
\description{
The \strong{ggeffects} package computes estimated marginal means (predicted values)
for the response, at the margin of specific values or levels from certain
model terms, i.e. it generates predictions by a model by holding the
non-focal variables constant and varying the focal variable(s).

Adjusted predictions or estimated marginal means by default always calculated
on the \emph{response} scale, which is the easiest and most intuitive scale to
interpret the results. There are other options for specific models, e.g. with
zero-inflation component (see documentation of the \code{type}-argument).

The result is returned as consistent data frame, which is nicely printed by
default. \code{plot()} can be used to easily create figures.
}
\note{
\strong{Printing Results}

The \code{print()} method gives a clean output (especially for predictions by
groups), and indicates at which values covariates were held constant.
Furthermore, the \code{print()} method has several arguments to customize the
output. See \href{https://strengejacke.github.io/ggeffects/articles/introduction_print.html}{this vignette}
for details.

\strong{Limitations}

The support for some models, for example from package \strong{MCMCglmm}, is
rather experimental and may fail for certain models. If you encounter
any errors, please file an issue \href{https://github.com/strengejacke/ggeffects/issues}{at Github}.
}
\section{Supported Models}{


A list of supported models can be found at \href{https://github.com/strengejacke/ggeffects}{the package website}.
Support for models varies by function, i.e. although \code{ggpredict()},
\code{ggemmeans()}, \code{ggaverage()} and \code{ggeffect()} support most models, some
models are only supported exclusively by one of the four functions. This means
that not all models work for every \code{marginalize} option of \code{predict_response()}.
}

\section{Difference between \code{ggpredict()} and \code{ggeffect()} or \code{ggemmeans()}}{


\code{ggpredict()} calls \code{predict()}, while \code{ggeffect()} calls \code{effects::Effect()}
and \code{ggemmeans()} calls \code{emmeans::emmeans()} to compute predicted values.
Thus, effects returned by \code{ggpredict()} can be described as \emph{conditional effects}
(i.e. these are conditioned on certain (reference) levels of factors), while
\code{ggemmeans()} and \code{ggeffect()} return \emph{marginal means}, since
the effects are "marginalized" (or "averaged") over the levels of factors
(or values of character vectors). Therefore, \code{ggpredict()} and \code{ggeffect()}
resp. \code{ggemmeans()} differ in how factors and character vectors are held
constant: \code{ggpredict()} uses the reference level (or "lowest" value in case
of character vectors), while \code{ggeffect()} and \code{ggemmeans()} compute a
kind of "average" value, which represents the proportions of each factor's
category. Use \code{condition} to set a specific level for factors in
\code{ggemmeans()}, so factors are not averaged over their categories,
but held constant at a given level.

Note that \code{ggpredict()} is equivalent to calling \code{predict_response()}, while
\code{ggeffect()} or \code{ggemmeans()} is equivalent to calling
\code{predict_response(marginalize = "marginalmeans")}.
}

\section{Difference between \code{ggemmeans()} and \code{ggaverage()}}{


Estimated marginal means, as computed by \code{ggemmeans()} or \code{ggeffect()}, are a
special case of predictions, made on a perfectly balanced grid of categorical
predictors, with numeric predictors held at their means, and marginalized with
respect to some focal variables. \code{ggaverage()} calculates predicted values
for each observation in the data multiple times, each time fixing all values
or levels of the focal terms to and then takes the average of these predicted
values (aggregated/grouped by the focal terms). In other words: while \code{ggemmeans()}
marginalizes over the levels of non-focal factors, \code{ggaverage()} marginalizes
non-focal terms over the observations in your sample (the available "empirical
data"). There is no rule of thumb which approach is better; it depends on the
characteristics of the sample and the population to which should be generalized.
Consulting the \href{https://marginaleffects.com/}{marginaleffects-website} might
help to decide which approach is more appropriate. The most apparent difference
is how \emph{non-focal} categorical predictors affect the predicted values. \code{ggpredict()}
will condition on a certain level of the non-focal factors (usually, the reference
level), \code{ggemmeans()} will "average" over the levels of non-focal factors,
while \code{ggaverage()} will average over the observations in your sample. See also
\href{https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html}{this vignette}
for details and examples.

Note that \code{ggaverage()} is equivalent to calling \code{predict_response(marginalize = "empirical")}.
}

\section{Holding covariates at constant values, or how marginalize over the \emph{non-focal} predictors}{


\code{predict_response()} is a wrapper around \code{ggpredict()}, \code{ggemmeans()} and
\code{ggaverage()}. Depending on the value of the \code{marginalize} argument,
\code{predict_response()} calls one of those functions, sometimes with different
arguments. The \code{marginalize} argument indicates how to marginalize over the
\emph{non-focal} predictors, i.e. those variables that are \emph{not} specified in
\code{terms}. Possible values are:
\itemize{
\item \code{"mean_reference"}: calls \code{ggpredict()}, i.e. non-focal predictors are set
to their mean (numeric variables) or reference level (factors, or "lowest"
value in case of character vectors). Technically, a data grid is constructed,
roughly comparable to \code{expand.grid()} on all unique combinations of
\code{model.frame(model)[, terms]}. This data grid (see \code{\link[=data_grid]{data_grid()}}) is used
for the \code{newdata} argument of \code{predict()}. In this case, all remaining
covariates that are not specified in \code{terms} are held constant: Numeric
values are set to the mean, integer values are set to their median, factors
are set to their reference level and character vectors to their mode (most
common element).
\item \code{"mean_mode"}: calls \code{ggpredict(typical = c(numeric = "mean", factor = "mode"))},
i.e. non-focal predictors are set to their mean (numeric variables) or mode
(factors, or "most common" value in case of character vectors).
\item \code{"marginalmeans"}: calls \code{ggemmeans()}, i.e. non-focal predictors are
set to their mean (numeric variables) or marginalized over the levels or
"values" for factors and character vectors. Marginalizing over the factor
levels of non-focal terms computes a kind of "weighted average" for the
values at which these terms are hold constant. Thus, non-focal categorical
terms are conditioned on "weighted averages" of their levels.
\item \code{"empirical"} (or \code{"counterfactual"} or \code{"ame"}): calls \code{ggaverage()}, i.e.
non-focal predictors are marginalized over the observations in your sample.
Technically, \code{ggaverage()} calculates predicted values for each observation
in the data multiple times (the data is duplicated once for all unique values
of the focal terms), each time fixing one unique value or level of the focal
terms and then takes the average of these predicted values (aggregated/grouped
by the focal terms). These kind of predictions are also called "counterfactual"
predictions (Dickerman and Hernan 2020). There is a more detailed description
in \href{https://strengejacke.github.io/ggeffects/articles/technical_differencepredictemmeans.html}{this vignette}.
}

For all the above options, the \emph{differences} between predicted values are
identical - if your main interest is to investigate "group differences" or
"inequalities", it doesn't matter much, which way you choose. However, if
you are specificall interested in the predicted values of your response, you
should consider the differences between the options. Predictions based on
\code{"mean_reference"} and \code{"mean_mode"} (aka \emph{conditional effects}) represent a
rather "theoretical" view on your data, which does not necessarily exactly
reflect the characteristics of your sample. \code{"marginalmeans"} (aka \emph{marginal effects})
comes closer to the sample, because it takes all possible values and  levels
of your non-focal predictors into account. \code{"empirical"} (aka \emph{average marginal effects})
is the most "realistic" approach, because it is based on the actual observations
in your sample.

You can set a default-option for the \code{marginalize} argument via \code{options()},
e.g. \code{options(ggeffects_marginalize = "empirical")}, so you don't have to
specify your "default" marginalization method each time you call \code{predict_response()}.
Use \code{options(ggeffects_marginalize = NULL)} to remove that setting.

The \code{condition} argument can be used to fix non-focal terms to specific
values.
}

\section{Marginal Effects and Adjusted Predictions at Specific Values}{


Meaningful values of focal terms can be specified via the \code{terms} argument.
Specifying meaningful or representative values as string pattern is the
preferred way in the \strong{ggeffects} package. However, it is also possible to
use a \code{list()} for the focal terms if prefer the "classical" R way, which is
described in \href{https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html}{this vignette}.

Indicating levels in square brackets allows for selecting only certain
groups or values resp. value ranges. The term name and the start of the
levels in brackets must be separated by a whitespace character, e.g.
\code{terms = c("age", "education [1,3]")}. Numeric ranges, separated with colon,
are also allowed: \code{terms = c("education", "age [30:60]")}. The stepsize for
ranges can be adjusted using \code{by}, e.g. \code{terms = "age [30:60 by=5]"}.

The \code{terms} argument also supports the same shortcuts as the \code{values} argument
in \code{values_at()}. So \code{terms = "age [meansd]"} would return predictions for
the values one standard deviation below the mean age, the mean age and one SD
above the mean age. \code{terms = "age [quart2]"} would calculate predictions at
the value of the lower, median and upper quartile of age.

Furthermore, it is possible to specify a function name. Values for predictions
will then be transformed, e.g. \code{terms = "income [exp]"}. This is useful when
model predictors were transformed for fitting the model and should be
back-transformed to the original scale for predictions. It is also possible
to define own functions (see
\href{https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html}{this vignette}).

Instead of a function, it is also possible to define the name of a variable
with specific values, e.g. to define a vector \code{v = c(1000, 2000, 3000)} and
then use \code{terms = "income [v]"}.

You can take a random sample of any size with \code{sample=n}, e.g
\code{terms = "income [sample=8]"}, which will sample eight values from
all possible values of the variable \code{income}. This option is especially
useful for plotting predictions at certain levels of random effects
group levels, where the group factor has many levels that can be completely
plotted. For more details, see
\href{https://strengejacke.github.io/ggeffects/articles/introduction_effectsatvalues.html}{this vignette}.

Finally, numeric vectors for which no specific values are given, a "pretty range"
is calculated (see \code{\link[=pretty_range]{pretty_range()}}), to avoid memory allocation problems
for vectors with many unique values. If a numeric vector is specified as
second or third term (i.e. if this vector represents a grouping structure),
representative values (see \code{\link[=values_at]{values_at()}}) are chosen (unless other values
are specified). If all values for a numeric vector should be used to compute
predictions, you may use e.g. \code{terms = "age [all]"}. See also package vignettes.

To create a pretty range that should be smaller or larger than the default
range (i.e. if no specific values would be given), use the \code{n} tag, e.g.
\code{terms="age [n=5]"} or \code{terms="age [n=12]"}. Larger values for \code{n} return a
larger range of predicted values.
}

\section{Bayesian Regression Models}{


\code{ggpredict()} also works with \strong{Stan}-models from the \strong{rstanarm} or
\strong{brms}-packages. The predicted values are the median value of all drawn
posterior samples. The confidence intervals for Stan-models are Bayesian
predictive intervals. By default (i.e. \code{ppd = FALSE}), the predictions are
based on \code{\link[rstantools:posterior_linpred]{rstantools::posterior_linpred()}} and hence have some limitations:
the uncertainty of the error term is not taken into account. The recommendation
is to use the posterior predictive distribution (\code{\link[rstantools:posterior_predict]{rstantools::posterior_predict()}}).
}

\section{Zero-Inflated and Zero-Inflated Mixed Models with brms}{


Models of class \code{brmsfit} always condition on the zero-inflation component,
if the model has such a component. Hence, there is no \code{type = "zero_inflated"}
nor \code{type = "zi_random"} for \code{brmsfit}-models, because predictions are based
on draws of the posterior distribution, which already account for the
zero-inflation part of the model.

\strong{Zero-Inflated and Zero-Inflated Mixed Models with glmmTMB}

If \code{model} is of class \code{glmmTMB}, \code{hurdle}, \code{zeroinfl} or \code{zerotrunc},
simulations from a multivariate normal distribution (see \code{?MASS::mvrnorm})
are drawn to calculate \code{mu*(1-p)}. Confidence intervals are then based on
quantiles of these results. For \code{type = "zi_random"}, prediction intervals
also take the uncertainty in the random-effect paramters into account (see
also \emph{Brooks et al. 2017}, pp.391-392 for details).

An alternative for models fitted with \strong{glmmTMB} that take all model
uncertainties into account are simulations based on \code{simulate()}, which
is used when \code{type = "simulate"} (see \emph{Brooks et al. 2017}, pp.392-393 for
details).
}

\section{MixMod-models from GLMMadaptive}{


Predicted values for the fixed effects component (\code{type = "fixed"} or
\code{type = "zero_inflated"}) are based on \code{predict(..., type = "mean_subject")},
while predicted values for random effects components (\code{type = "random"} or
\code{type = "zi_random"}) are calculated with \code{predict(..., type = "subject_specific")}
(see \code{?GLMMadaptive::predict.MixMod} for details). The latter option
requires the response variable to be defined in the \code{newdata}-argument
of \code{predict()}, which will be set to its typical value (see
\code{\link[=values_at]{values_at()}}).
}

\section{Multinomial Models}{


\code{polr}, \code{clm} models, or more generally speaking, models with ordinal or
multinominal outcomes, have an additional column \code{response.level}, which
indicates with which level of the response variable the predicted values are
associated.
}

\examples{
\dontshow{if (requireNamespace("sjlabelled") && requireNamespace("ggplot2")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(sjlabelled)
data(efc)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

ggpredict(fit, terms = "c12hour")
ggpredict(fit, terms = c("c12hour", "c172code"))
ggpredict(fit, terms = c("c12hour", "c172code", "c161sex"))

# specified as formula
ggpredict(fit, terms = ~ c12hour + c172code + c161sex)

# only range of 40 to 60 for variable 'c12hour'
ggpredict(fit, terms = "c12hour [40:60]")

# terms as named list
ggpredict(fit, terms = list(c12hour = 40:60))

# covariate "neg_c_7" is held constant at a value of 11.84 (its mean value).
# To use a different value, use "condition"
ggpredict(fit, terms = "c12hour [40:60]", condition = c(neg_c_7 = 20))

# to plot ggeffects-objects, you can use the 'plot()'-function.
# the following examples show how to build your ggplot by hand.

\donttest{
# plot predicted values, remaining covariates held constant
library(ggplot2)
mydf <- ggpredict(fit, terms = "c12hour")
ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)

# three variables, so we can use facets and groups
mydf <- ggpredict(fit, terms = c("c12hour", "c161sex", "c172code"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet, ncol = 2)

# select specific levels for grouping terms
mydf <- ggpredict(fit, terms = c("c12hour", "c172code [1,3]", "c161sex"))
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~facet) +
  labs(
    y = get_y_title(mydf),
    x = get_x_title(mydf),
    colour = get_legend_title(mydf)
  )

# level indication also works for factors with non-numeric levels
# and in combination with numeric levels for other variables
data(efc)
efc$c172code <- sjlabelled::as_label(efc$c172code)
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
ggpredict(fit, terms = c("c12hour",
  "c172code [low level of education, high level of education]",
  "c161sex [1]"))

# when "terms" is a named list
ggpredict(fit, terms = list(
  c12hour = seq(0, 170, 30),
  c172code = c("low level of education", "high level of education"),
  c161sex = 1)
)

# use categorical value on x-axis, use axis-labels, add error bars
dat <- ggpredict(fit, terms = c("c172code", "c161sex"))
ggplot(dat, aes(x, predicted, colour = group)) +
  geom_point(position = position_dodge(0.1)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(0.1)
  ) +
  scale_x_discrete(breaks = 1:3, labels = get_x_labels(dat))

# 3-way-interaction with 2 continuous variables
data(efc)
# make categorical
efc$c161sex <- as_factor(efc$c161sex)
fit <- lm(neg_c_7 ~ c12hour * barthtot * c161sex, data = efc)
# select only levels 30, 50 and 70 from continuous variable Barthel-Index
dat <- ggpredict(fit, terms = c("c12hour", "barthtot [30,50,70]", "c161sex"))
ggplot(dat, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~facet) +
  labs(
    colour = get_legend_title(dat),
    x = get_x_title(dat),
    y = get_y_title(dat),
    title = get_title(dat)
  )

# or with ggeffects' plot-method
plot(dat, ci = FALSE)
}

# predictions for polynomial terms
data(efc)
fit <- glm(
  tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
  data = efc,
  family = poisson()
)
ggeffect(fit, terms = "e17age")
\dontshow{\}) # examplesIf}
}
\references{
\itemize{
\item Brooks ME, Kristensen K, Benthem KJ van, Magnusson A, Berg CW, Nielsen A,
et al. glmmTMB Balances Speed and Flexibility Among Packages for Zero-inflated
Generalized Linear Mixed Modeling. The R Journal. 2017;9: 378-400.
\item Johnson PC, O'Hara RB. 2014. Extension of Nakagawa & Schielzeth's R2GLMM
to random slopes models. Methods Ecol Evol, 5: 944-946.
\item Dickerman BA, Hernan, MA. Counterfactual prediction is not only for causal
inference. Eur J Epidemiol 35, 615–617 (2020).
}
}