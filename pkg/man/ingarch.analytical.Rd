\name{ingarch.analytical}
\alias{ingarch.analytical}
\alias{ingarch.mean}
\alias{ingarch.acf}
\alias{ingarch.var}
\title{
Analytical Mean, Variance and Autocorrelation of an INGARCH Process 
}
\description{
Functions to calculate the analytical mean, variance and autocorrelation/partial autocorrelation/autocovariance function of an INGARCH process.
}
\usage{
ingarch.mean(intercept, past_obs, past_mean)
ingarch.var(intercept, past_obs, past_mean)
ingarch.acf(intercept, past_obs, past_mean, lag.max=10,
        type=c("acf", "pacf", "acvf"), plot=TRUE, ...)
}

\arguments{
  \item{intercept}{
  numeric positive value for the intercept \eqn{\beta_0}{\beta[0]}.
}
  \item{past_obs}{
  numeric non-negative vector containing the coefficients \eqn{\beta_1,\ ldots, \beta_p}{\beta[1], \ldots, \beta[p]} for regression on previous observations (see Details).
}
  \item{past_mean}{
  numeric non-negative vector containing the coefficients \eqn{\alpha_1,\ ldots, \alpha_q}{\alpha[1], \ldots, \alpha[q]} for regression on previous conditional means (see Details).
}
  \item{lag.max}{
  integer value indicating how many lags of the (partial) autocorrelation/autocovariance should be calculated.
}
  \item{type}{
  character. If \code{type="acf"} (the default) the autocorrelation function is calculated, \code{"pacf"} gives the partial autocorrelation function and \code{"acvf"} the autocovariance function.
}
  \item{plot}{
  logical. If \code{plot=TRUE} (the default) the values are plotted and returned invisible. 
}
  \item{...}{
  additional arguments to be passed to function \code{\link{plot}}.
  }
}

\details{
  The INGARCH(\eqn{p},\eqn{q}) model used here follows the definition
    \deqn{Z_{t}|{\cal{F}}_{t-1} \sim Poi(\kappa_{t}),}{
    Z[t]|F[t-1] ~ Poi(\kappa[t]),}
  where \eqn{{\cal{F}}_{t-1}}{F[t-1]} is the \eqn{\sigma}-field containing the information about the past of the process up to time \eqn{t-1}.
  The conditional mean is given by
    \deqn{\kappa_t = \beta_0 + \beta_1 Z_{t-1} + \ldots + \beta_p Z_{t-p}
    + \alpha_1 \kappa_{t-1} + \ldots  + \alpha_q \kappa_{t-q}.}{
    \kappa[t] = \beta[0] + \beta[1] Z[t-1] + \ldots + \beta[p] Z[t-p]
    + \alpha[1] \kappa[t-1] + \ldots  + \alpha[q] \kappa[t-q].}
    The function \code{ingarch.acf} depends on the function \code{\link[ltsa:tacvfARMA]{tacvfARMA}} from package \code{ltsa}, which needs to be installed.
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link{ingarch}} for fitting a more general INGARCH model with covariates and \code{\link{ingarch.sim}} for simulation from such a model.
}

\examples{
ingarch.mean(0.3, c(0.1,0.1), 0.1)
ingarch.var(0.3, c(0.1,0.1), 0.1)
ingarch.acf(0.3, c(0.1,0.1,0.1), 0.1, type="acf", lag.max=15)
}

\keyword{INGARCH}
\keyword{mean}
\keyword{variance}
\keyword{autocorrelation}
