\name{predict.tsglm}
\alias{predict.tsglm}
\title{
Predict Method for Time Series of Counts Following Generalised Linear Models
}
\description{
Predict future observations based on a fitted GLM-type model for time series of counts.
}
\usage{
\method{predict}{tsglm}(object, n.ahead = 1, newobs = NULL, newxreg = NULL, ...)
}
\arguments{
  \item{object}{
an object of class \code{"tsglm"}. Usually the result of a call to \code{\link{tsglm}}.
}
  \item{n.ahead}{
positive integer value giving the number of steps ahead for which prediction is required.
}
  \item{newobs}{
integer vector of known future observations of the time series. This argument is only relevant if more than one observation ahead is to be predicted (\code{n.ahead} greater than 1). The \eqn{h}-step-ahead prediction for \eqn{h>1} is computed as a 1-step-ahead prediction given all previous values, which can be observations of the original time series or new observations provided in this argument. Previous observations which are not available are replaced by their respective 1-step-ahead prediction.
}
  \item{newxreg}{
matrix or vector containing new values for the covariates to be used for prediction. If \code{newxreg} is omitted or contains less rows than the value of \code{n.ahead}, the last known values of the covariates are used for prediction. This is often not reasonable and it is strongly advised to explicitely make assumptions on future covariates and to specify the argument \code{xreg} accordingly. 
}
  \item{...}{
further arguments are currently ignored. Only for compatibility with generic function.
}
}
\details{
Returns predictions for the \code{n.ahead} observations following the fitted time series contained in argument \code{object}. The 1-step-ahead prediction is the conditional expectation of the observation to be predicted. The true parameters are replaced by their estimations given in argument \code{object}. For a 2-step-prediction the true previous observation is used when given in argument \code{newobs}, otherwise it is replaced by the 1-step-ahead prediction computed before. For a 3-step-prediction this holds for the previous two observations, which are replaced by their respective predictions if not available, and so on.
}
\value{
Numeric vector of predictions. Has class \code{"ts"} if the response used for fitting had this class.
}

\author{
Tobias Liboschik and Philipp Probst
}

\seealso{
\code{\link{tsglm}} for fitting a GLM for time series of counts.
}

\examples{
campy_fit <- tsglm(ts=campy, model=list(past_obs=1, past_mean=c(7,13)),
                    link="identity")
predict(campy_fit, n.ahead=5)
}

\keyword{GLM}
\keyword{INGARCH}
\keyword{predict}
