\name{interv_detect.ingarch}
\alias{interv_detect.ingarch}
\alias{interv_detect}

\title{
Detecting an Intervention in INGARCH Processes
}

\description{
Detection procedure for an intervention of given type occuring at unknown time.
}

\usage{
\method{interv_detect}{ingarch}(fit, taus=2:length(ts), delta, external=FALSE, 
              B=NULL, info=c("score", "hessian"), init.control_bootstrap, 
              final.control_bootstrap, inter.control_bootstrap, 
              parallel=FALSE, est_interv=TRUE, ...)
}

\arguments{
  \item{fit}{
an object of class \code{"tsglm"}. Usually the result of a call to \code{\link{tsglm}}.
}
  \item{taus}{
  integer vector of times which are considered for the possible intervention to occur.
  Default is to consider all times.
}
  \item{delta}{
  numeric value that determines the type of intervention (see Details).
}
  \item{external}{
  logical value specifying wether the interventions effect is external or not (see Details).
} 
  \item{B}{
  positive integer value giving the number of bootstrap samples for estimation of the p-value. For \code{B=NULL} (the default) no p-value is returned.
}
  \item{info}{
  character value that determines how to calculate the information matrix, see \code{\link{tsglm}}.
}
  \item{init.control_bootstrap}{
  named list that determines how to make initial estimation in the bootstrap, see argument \code{init.control} in \code{\link{tsglm}}. If missing the same settings as for the regular estimation are used.
}
  \item{final.control_bootstrap}{
  named list that determines how to make final maximum likelihood estimation in the bootstrap, see argument \code{final.control} in \code{\link{tsglm}}. If missing the same settings as for the regular estimation are used.
}
  \item{inter.control_bootstrap}{
  named list  determining how to maximise the log-likelihood function in an intermediate step, see argument \code{inter.control} in \code{\link{tsglm}}. If missing the same settings as for the regular estimation are used.
}
  \item{parallel}{
  logical value. If \code{parallel=TRUE}, the bootstrap is distributed to multiple cores parallely. Requires a computing cluster to be initialised by \code{\link[snowfall:sfInit]{sfInit}} from package \code{snowfall}. 
}
  \item{est_interv}{
  logical value. If \code{est_interv=TRUE} a fit for the model with the intervention effect with the largest test statistic is computed and additionally returned.
}
  \item{...}{
  additional arguments passed to the fitting function \code{\link{tsglm}}.
}
}
 
\details{
For each time in \code{taus} the test statistic of a score test on an intervention effect occuring at that time is computed, see \code{\link[=interv_test.ingarch]{interv_test}}. The time with the maximum test statistic is considered as a candidate for a possible intervention effect at that time. The type of the intervention effect is specified by \code{delta} as described in \code{\link{interv_covariate}}.  The intervention is included as an additional covariate according to the definition in \code{\link{tsglm}} and can have an internal (the default) or external \code{external=TRUE} effect.

If argument \code{B} is not \code{NULL}, the null hypothesis that there is no intervention effect at any time is tested. Test statistic for this test is the maximum test statistic of the score test (see above). The p-value is computed by a parametric bootstrap with \code{B} bootstrap samples. It is recommended to use at least several hundred bootstrap samples. Note that this bootstrap procedure is very time-consuming.
}

\value{
  An object of class \code{"interv_detect"}, which is a list with at least the following components:

  \item{test_statistic}{
  maximum value of the score test statistics for all considered times in \code{taus}.
}
  \item{test_statistic_tau}{
  numeric vector of all score test statistics at the considered times in \code{taus}.
}
  \item{tau_max}{
  time at which the score test statistic has its maximum.
}
  \item{fit_H0}{
  object of class \code{"tsglm"} with the fitted model under the null hypothesis of no intervention, see \code{\link{tsglm}}.
}
  \item{model_interv}{
  model specification of the model with the specified intervention at time \code{tau_max}.
}

If argument \code{est_interv=TRUE} (the default), the following component is additionally returned:

  \item{fit_interv}{
  object of class \code{"tsglm"} with the fitted model with the specified intervention at time \code{tau_max}, see \code{\link{tsglm}}.
}
}

\references{
Fokianos, K. and Fried, R. (2010) Interventions in INGARCH processes. \emph{Journal of Time Series Analysis} \bold{31(3)}, 210--225, \url{http://dx.doi.org/10.1111/j.1467-9892.2010.00657.x}.

Liboschik, T., Kerschke, P., Fokianos, K. and Fried, R. (2013) Modelling interventions in INGARCH processes. \emph{SFB 823 Discussion Paper} \bold{03/13}, \url{http://hdl.handle.net/2003/29878}.
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
S3 methods \code{print} and \code{\link[=plot.interv_detect]{plot}}.

\code{\link{tsglm}} for fitting an INGARCH model.
\code{\link[=interv_test.ingarch]{interv_test}} for testing on intervention effects and \code{\link[=interv_multiple.ingarch]{interv_multiple}} for iterative detection of multiple interventions of unknown types. \code{\link{interv_covariate}} for generation of deterministic covariates describing intervention effects.
}

\examples{
#Searching for a potential intervention effect:
#campy_fit <- tsglm(ts=campy, model=list(past_obs=1, past_mean=c(7,13)))
#campy_intervdetect <- interv_detect.ingarch(fit=campy_fit, taus=80:120,
#                            delta=1, external=FALSE)
#campy_intervdetect
#plot(campy_intervdetect)
}