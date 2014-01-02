\name{ingarch.sim}
\alias{ingarch.sim}
\title{
Simulate from an INGARCH Model
}
\description{
Generates a simulated time series from an INGARCH model.
}
\usage{
ingarch.sim(n, param=list(intercept=1, past_obs=NULL, past_mean=NULL, xreg=NULL), 
            model=list(past_obs=NULL, past_mean=NULL, xreg=NULL, external=FALSE), 
            distr=c("poisson", "nbinom"), distrcoefs, fit, n_start=50)
}
\arguments{
  \item{n}{
  integer value giving the number of observations to be simulated.
}
  \item{param}{
  a named list giving the parameters of the INGARCH model with the following elements: 
    \describe{
      \item{intercept}{numeric positive value for the intercept \eqn{\beta_0}{\beta[0]}.}
      \item{past_obs}{numeric non-negative vector containing the coefficients \eqn{\beta_1,\ ldots, \beta_p}{\beta[1], \ldots, \beta[p]} for regression on previous observations (see Details).}
      \item{past_mean}{numeric non-negative vector containing the coefficients \eqn{\alpha_1,\ ldots, \alpha_q}{\alpha[1], \ldots, \alpha[q]} for regression on previous conditional means (see Details).}
      \item{xreg}{numeric non-negative vector specifying the size \eqn{\nu_1,\ ldots, \nu_r}{\nu[1], \ldots, \nu[r]} of each intervention}   
  }
}
  \item{model}{
  a named list specifying the INGARCH model with the elements \code{past_obs}, \code{past_mean}, \code{xreg} and \code{external} (see function \code{\link{ingarch}} for details). This model specification must be in accordance to the parameters given in argument \code{param} (see Details).
}
  \item{distr}{
  character giving the conditional distribution. Default is a Poisson distribution. Currently the only alternative is the negative binomial distribution.
}
  \item{distrcoefs}{
  numeric vector of additional coefficients specifying the conditional distribution. For \code{distr="poisson"} no additional parameters need to be provided. 
}
  \item{fit}{
  an object of class \code{"ingarch"}. Usually the result of a call to \code{\link{ingarch}}. If argument \code{fit} is not missing, the model specification and estimated parameters from this argument are used instead of those in arguments \code{model} and \code{param}. The length of the simulated time seriesis only taken from argument \code{fit}, if no argument \code{n} is provided.  The same holds for arguments \code{distr} and \code{distrcoefs}, which are also prefered over the respective information provided in argument \code{fit}.
}
  \item{n_start}{
  number of observations used as a burn-in.
}
}

\details{
The definition of an INGARCH model with covariates used here is like in function \code{\link{ingarch}}.
}

\value{
  A list with the following components:

\item{\code{ts}}{
  an object of class \code{"ts"} with the simulated time series.
}
\item{\code{mean}}{
  an object of class \code{"ts"} with the simulated conditional means \eqn{\kappa_t}{\kappa[t]} for all \eqn{t=1, \ldots, n}.
}
\item{\code{xreg_effect}}{
  an object of class \code{"ts"} with the cumulated effect of the covariates \eqn{\eta_1 X_{t,1} + \ldots + \eta_r X_{t,r}}{\eta[1] X[t,1] + \ldots + \eta[r] X[t,r]} for all \eqn{t=1, \ldots, n}.
}
}

\references{
Christou, V. and Fokianos, K. (2013) Quasi-likelihood inference for negative binomial time series models. \emph{Journal of Time Series Analysis} (published online), \url{http://dx.doi.org/10.1002/jtsa.12050}.

Ferland, R., Latour, A. and Oraichi, D. (2006) Integer-valued GARCH process. \emph{Journal of Time Series Analysis} \bold{27(6)}, 923--942, \url{http://dx.doi.org/10.1111/j.1467-9892.2006.00496.x}.

Fokianos, K. and Fried, R. (2010) Interventions in INGARCH processes. \emph{Journal of Time Series Analysis} \bold{31(3)}, 210--225, \url{http://dx.doi.org/10.1111/j.1467-9892.2010.00657.x}.

Fokianos, K., Rahbek, A. and Tjostheim, D. (2009) Poisson autoregression. \emph{Journal of the American Statistical Association} \bold{104(488)}, 1430--1439, \url{http://dx.doi.org/10.1198/jasa.2009.tm08270}.

Liboschik, T., Kerschke, P., Fokianos, K. and Fried, R. (2013) Modelling interventions in INGARCH processes. \emph{SFB 823 Discussion Paper} \bold{03/13}, \url{http://hdl.handle.net/2003/29878}.
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
 \code{\link{ingarch}} for fitting an INGARCH model.
}

\examples{
#Simulate from an INGARCH model with two interventions:
interventions <- interv_covariate(n=200, tau=c(50, 150), delta=c(1, 0.8))
model <- list(past_obs=1, past_mean=c(1, 7), xreg=interventions, external=FALSE)
param <- list(intercept=2, past_obs=0.3, past_mean=c(0.2, 0.1), xreg=c(3, 10))
ingarch.sim(n=200, param=param, model=model, distr="poisson")
}

\keyword{INGARCH}
\keyword{simulation} 
