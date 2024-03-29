% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residualize_over_grid.R
\name{residualize_over_grid}
\alias{residualize_over_grid}
\alias{residualize_over_grid.data.frame}
\alias{residualize_over_grid.ggeffects}
\title{Compute partial residuals from a data grid}
\usage{
residualize_over_grid(grid, model, ...)

\method{residualize_over_grid}{data.frame}(grid, model, predictor_name, ...)

\method{residualize_over_grid}{ggeffects}(grid, model, protect_names = TRUE, ...)
}
\arguments{
\item{grid}{A data frame representing the data grid, or an object of class
\code{ggeffects}, as returned by \code{predict_response()}.}

\item{model}{The model for which to compute partial residuals. The data grid
\code{grid} should match to predictors in the model.}

\item{...}{Currently not used.}

\item{predictor_name}{The name of the focal predictor, for which partial residuals
are computed.}

\item{protect_names}{Logical, if \code{TRUE}, preserves column names from the
\code{ggeffects} objects that is used as \code{grid}.}
}
\value{
A data frame with residuals for the focal predictor.
}
\description{
This function computes partial residuals based on a data grid,
where the data grid is usually a data frame from all combinations of factor
variables or certain values of numeric vectors. This data grid is usually used
as \code{newdata} argument in \code{predict()}, and can be created with
\code{\link[=new_data]{new_data()}}.
}
\section{Partial Residuals}{

For \strong{generalized linear models} (glms), residualized scores are
computed as \code{inv.link(link(Y) + r)} where \code{Y} are the predicted
values on the response scale, and \code{r} are the \emph{working} residuals.
\cr\cr
For (generalized) linear \strong{mixed models}, the random effect are also
partialled out.
}

\examples{
library(ggeffects)
set.seed(1234)
x <- rnorm(200)
z <- rnorm(200)
# quadratic relationship
y <- 2 * x + x^2 + 4 * z + rnorm(200)

d <- data.frame(x, y, z)
model <- lm(y ~ x + z, data = d)

pr <- predict_response(model, c("x [all]", "z"))
head(residualize_over_grid(pr, model))
}
\references{
Fox J, Weisberg S. Visualizing Fit and Lack of Fit in Complex Regression
Models with Predictor Effect Plots and Partial Residuals. Journal of Statistical
Software 2018;87.
}
