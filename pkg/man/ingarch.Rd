\name{ingarch}
\alias{ingarch}
\title{
INGARCH Modelling of Time Series
}
\description{
The function \code{ingarch} fits an INGARCH model to a univariate time series. An INGARCH model is a generalised linear model (GLM) for time series. It has a linear conditional mean specification regressing on past observations, past conditional means and covariates as defined below. The conditional distributions can currently be either Poisson or negative binomial. Estimation is done by conditional maximum likelihood for the Poisson distribution and a conditional quasi-likelihood approach based on the Poisson likelihood function for the negative binomial distribution.

The function \code{ingarch.fit} is a lower level function to fit the mean specification of an INGARCH model assuming a Poisson distribution. It is called by \code{ingarch}. It has additional arguments allowing for a finer control of the fitting procedure, which can be handed over to the function \code{ingarch} by its \code{...} argument.
}
\usage{
ingarch(ts, model=list(past_obs = NULL, past_mean = NULL,
        xreg = NULL, external = NULL), distr=c("poisson", "nbinom"), ...)
        
%ingarch.fit(ts, model = list(past_obs = NULL, past_mean = NULL,
%            xreg = NULL, external = NULL), score = TRUE,
%            info = c("score", "none", "hessian"),
%            init=c("marginal", "zero", "firstobs"),
%            epsilon = 1e-06, slackvar = 1e-06, init.control = list(),
%            final.control = list(), inter.control = NULL) 
}


\arguments{
  \item{ts}{
    a univariate time series.
  }
  \item{model}{
    a named list specifying the model for the conditional mean, which can have the following elements:
    \describe{  
      \item{\code{past_obs}}{integer vector giving the previous observations to be regressed on (autoregression). This is a vector with the elements \eqn{i_1,\ldots,i_p}{i[1],\ldots,i[p]} (see Details). If omitted or of length zero there will be no regression on previous observations.}    
      \item{\code{past_mean}}{integer vector giving the previous conditional means to be regressed on. This is a vector with the elements \eqn{j_1,\ldots,j_q}{j[1],\ldots,j[q]} (see Details). If omitted or of length zero there will be no regression on previous conditional means.}
      \item{\code{xreg}}{matrix with non-negative covariates in the columns, i.e. its number of rows must be \code{length(ts)}. This is the matrix \eqn{X} (see Details). If omitted no covariates will be included.} 
      \item{\code{external}}{logical vector of length \code{ncol(xreg)} specifying for each covariate wether its effect is external or not (see Details). If this is only a scalar this choice will be used for all covariates. If omitted all covariates will have an internal effect (i.e. \code{external=FALSE}).}
    } 
  }
  \item{distr}{
    character giving the conditional distribution. Default is a Poisson distribution. Currently the only alternative is the negative binomial distribution.
  }
  \item{...}{
    additional arguments to be passed to the lower level fitting function \code{ingarch.fit}. See below.  
  }
%  \item{score}{
%    logical value indicating whether the score vector should be computed.
%  }
%  \item{info}{
%    character that determine if and how to compute the information matrix. Can be set to \code{"score"} (the default) for calculation via the outer product of the score vector or to \code{"hessian"} for calculation via the Hessian matrix of second derivatives. If set to \code{"none"} no information matrix is computed.
%  }
%  \item{init}{
%    character that determines how the recursion of the conditional mean (and possibly of its derivatives) is initialised. If \code{init="marginal"} (the default) the marginal mean of a model without covariates and its derivatives are used. If \code{init="zero"} all values are initialised by zero, wheras for \code{init="firstobs"} the first obersvation is used.
%  }
%  \item{epsilon}{
%    numeric positive but small value determining how close parameters may come to the limits of the parameter space.
%  }
%  \item{slackvar}{
%    numeric positive but small value determining how true inequalities among the parameter restrictions are treated; a true inequality \code{x < y} will be transformed to \code{x + slackvar <= y}.
%  }
%  \item{init.control}{
%    named list with optional elements that determine how to make initial estimation. Possible list elements are:
%    \describe{  
%      \item{\code{use}}{integer vector of length one or two giving the number of observations from the beginning (if of length one) or the range of observations (if of length two) used for initial estimation. For \code{use = Inf} all observations are used, which is the default when omitted.}
%      \item{\code{method}}{character specifying how initial estimators should be estimated. Possible values are \code{"CSS"}, \code{"CSS-ML"}, \code{"ML"}, \code{"MM"}, \code{"GLM"} and \code{"fixed"}. 
%  If \code{method} is \code{"CSS"}, \code{"CSS-ML"} or \code{"ML"}, the initial estimate is based on an ARMA fit using the function \code{\link{arima}}, and list element \code{method} is passed to its argument of the same name.  
%  If \code{method="MM"}, the initial estimate is an ARMA(1,1) fit by moment estimators and parameters of higher order than one are set to zero. For this method the initial parameter values for the covariates are 0 by default and can be set in the list element \code{xreg}.
%  If \code{method="GLM"}, the estimated parameters of a generalised linear model with regression on the specified past observations and covariates, but not on past conditional means, are used as initial estimates. Initial estimates for the coefficients of past conditional means are set to zero.
%  If \code{method="fixed"}, parameters given in further named list elements of \code{init.control} are used when available, else predefined values are used.}
%      \item{\code{intercept}}{numeric value with initial value for the intercept parameter. Default value is 1.}
%      \item{\code{past_obs}}{numeric vector with initial value for parameters for regression on previous observations. Default values are 0.}
%      \item{\code{past_mean}}{numeric vector with initial value for parameters for regression on previous conditional means. Default values are 0.}
%      \item{\code{xreg}}{numeric vector with initial value for the regression parameters. These values will also be used if \code{method="MM"}. Default values are 0.}
%    }  
%  }
%  \item{final.control}{
%    named list with optional elements that determine how to make the final maximum likelihood estimation. If \code{final.control=NULL} only initial estimates are computed. Possible list elements are:
%    \describe{  
%      \item{\code{constrained}}{named list whose element are passed to function \code{\link{constrOptim}} with possible elements \code{mu}, \code{outer.iterations} and \code{outer.eps} (see \code{\link{constrOptim}} for details). If \code{constrained=NULL} an unconstrained optimisation is made with function \code{\link{optim}}. Note that this is likely to result in a non-stationary fitted model which might cause further problems.}
%      \item{\code{optim.method}}{character which is passed to functions \code{\link{constrOptim}} or \code{\link{optim}} as argument \code{method}.}
%      \item{\code{optim.control}}{named list which is passed to function \code{\link{constrOptim}} or \code{\link{optim}} as argument \code{control}. Must not contain the list element \code{fnscale}.}
%    }
%  }
%  \item{inter.control}{
%    named list determining how to maximise the log-likelihood function in a first step. This intermediate optimisation will start from the initial estimation and be followed by the final optimisation, which will in turn start from the intermediate optimisation result. This intermediate optimisation is intended to use a very quick but imprecise optimisation algorithm. Possible elements are the same as for \code{final.control}. The default is \code{inter.control=NULL}, which skips this intermediate optimisation step. 
%  }
}

\details{
  The INGARCH model used here follows the definition
    \deqn{Z_{t}|{\cal{F}}_{t-1} \sim \mathrm{Poi}(\kappa_{t}) \quad \mathrm{or} \quad \mathrm{NegBin}(\kappa_{t}, \nu),}{
    Z[t]|F[t-1] ~ Poi(\kappa[t]) or  NegBin(\kappa[t], \nu),}
  where \eqn{{\cal{F}}_{t-1}}{F[t-1]} is the \eqn{\sigma}-field containing the information about the past of the process up to time \eqn{t-1}.
  For the model with covariates having an internal effect (the default) the conditional mean is given by
    \deqn{\kappa_t = \beta_0 + \beta_1 Z_{t-i_1} + \ldots + \beta_p Z_{t-i_p}
    + \alpha_1 \kappa_{t-j_1} + \ldots  + \alpha_q \kappa_{t-j_q}
    + \eta_1 X_{t,1} + \ldots + \eta_r X_{t,r},}{
    \kappa[t] = \beta[0] + \beta[1] Z[t-i[1]] + \ldots + \beta[p] Z[t-i[p]]
    + \alpha[1] \kappa[t-j[1]] + \ldots  + \alpha[q] \kappa[t-j[q]]
    + \eta[1] X[t,1] + \ldots + \eta[r] X[t,r],}
  whereas for the model with external effects of the covariates the conditional mean is given by
    \deqn{\kappa_t = \lambda_t + \eta_1 X_{t,1} + \ldots + \eta_r X_{t,r}}{
    \kappa[t] = \lambda[t] + \eta[1] X[t,1] + \ldots + \eta[r] X[t,r]}
    \deqn{\lambda_t = \beta_0 + \beta_1 Z_{t-i_1} + \ldots + \beta_p Z_{t-i_p}
    + \alpha_1 \lambda_{t-j_1} + \ldots  + \alpha_q \lambda_{t-j_q},}{
    \lambda[t] = \beta[0] + \beta[1] Z[t-i[1]] + \ldots + \beta[p] Z[t-i[p]
    + \alpha[1] \lambda[t-j[1]] + \ldots  + \alpha[q] \lambda[t-j[q]],}  
  It is also possible to model a combination of external and internal covariates.
  
  The Poisson distribution is parametrised by the mean \code{lambda} according to the definition in \code{\link{Poisson}}.
  The negative binomial distribution is parametrised by the mean \code{mu} with an additional dispersion parameter \code{size} according to the definition in \code{\link{NegBinomial}}. In the notation above its mean parameter \code{mu} is \eqn{\kappa_{t}}{\kappa[t]} and its dispersion parameter \code{size} is \eqn{\nu}.
}

\value{
A fit to an INGARCH time series model with the function \code{ingarch} has the primary class \code{"ingarch"} and the secondary class \code{"tsglm"}. The class \code{"tsglm"} indicates the output of a fit to a GLM-type time series model. INGARCH models are a special case of a GLM-type time series model and have the class \code{"ingarch"}. There are S3 methods available for all GLM-type time series model fits with class \code{"tsglm"}, and others only for INGARCH models with class \code{"ingarch"}.

An object of class \code{"ingarch"} is a list with at least the following elements:

  \item{coefficients}{a named vector of the maximum likelihood estimated coefficients, which can be extracted by the \code{\link{coef}} method.}
  
  \item{init}{a named vector of the estimated initial coefficients.}
  
  \item{residuals}{a vector of residuals, which can be extracted by the \code{\link[=residuals.tsglm]{residuals}} method.}

  \item{fitted.values}{the fitted values, which can be extracted by the \code{\link{fitted}} method.}

  \item{loglik}{the log-likelihood of the fitted model, which can be extracted by the \code{\link{logLik}} method.}

  \item{score}{the score vector at the maximum likelihood estimation.}

  \item{info.matrix}{the information matrix at the maximum likelihood estimation assuming a Poisson distribution.}

  \item{info.matrix}{the information matrix at the maximum likelihood estimation assuming the distribution specified in \code{distr}.}

  \item{call}{the matched call.}

  \item{n_obs}{the number of observations.}

  \item{ts}{the original time series.}

  \item{model}{the model specification.}
  
  \item{distr}{a character giving the fitted conditional distribution.}
  
  \item{distrcoefs}{a named vector of the estimated additional coefficients specifying the conditional distribution.}
  
  \item{sigmasq}{the estimated variance coefficient.}

The function \code{ingarch.fit} has the same output except the elements \code{distr}, \code{distrcoefs} and \code{sigmasq}. In addition to that, it returns the following list elements:

  \item{inter}{some details on the intermediate estimation of the coefficients as returned by \code{\link{constrOptim}} or \code{\link{optim}}.} 

  \item{final}{some details on the final estimation of the coefficients as returned by \code{\link{constrOptim}} or \code{\link{optim}}.}

  \item{durations}{named vector with the durations of the model fit in seconds.}
  
  \item{outerscoreprod}{array of outer products of score vectors at each time point.} 

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
S3 methods \code{\link[=summary.ingarch]{summary}}, \code{\link{vcov}} and \code{\link[=predict.ingarch]{predict}} for the class \code{"ingarch"}.

S3 methods \code{\link{print}}, \code{\link{fitted}}, \code{\link[=residuals.tsglm]{residuals}}, \code{\link{coef}}, \code{\link{logLik}}, \code{\link{AIC}} and \code{\link{BIC}} for the class \code{"tsglm"}. Additionally, there are the S3 methods \code{\link[=pit.tsglm]{pit}}, \code{\link[=marcal.tsglm]{marcal}} and \code{\link[=scoring.tsglm]{scoring}} for predictive model assessment.

\code{\link{ingarch.se}} for standard errors of the coefficients and \code{\link{ingarch.sim}} for simulation from an INGARCH model.
S3 methods \code{\link[=interv_test.ingarch]{interv_test}}, \code{\link[=interv_detect.ingarch]{interv_detect}} and \code{\link[=interv_multiple.ingarch]{interv_multiple}} for tests on and detection of intervention effects.
\code{\link{ingarch.mean}}, \code{\link{ingarch.var}} and \code{\link{ingarch.acf}} for calculation of analytical mean, variance and autocorrelation function of an INGARCH model without covariates.

Example time series of counts are \code{\link{campy}}, \code{\link{ecoli}}, \code{\link{ehec}}, \code{\link{influenza}}, \code{\link{measles}} in this package, \code{\link[gamlss.data]{polio}} in package \code{gamlss.data}.
}

\examples{
seatbelts_fit <- ingarch(ts=Seatbelts[, "DriversKilled"],
  model=list(past_obs=1, past_mean=c(1, 12), xreg=Seatbelts[, "PetrolPrice"]), distr="poisson")
summary(seatbelts_fit)
plot(seatbelts_fit)
}

\keyword{INGARCH}
\keyword{fit}
\keyword{model}
\keyword{time series}
\keyword{count data}
