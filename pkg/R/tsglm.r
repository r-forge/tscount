tsglm <- function(ts, model=list(past_obs=NULL, past_mean=NULL, xreg=NULL, external=NULL), link=c("identity", "log"), distr=c("poisson", "nbinom"), ...){
  link <- match.arg(link)  
  distr <- match.arg(distr)
  if(link=="log" && distr=="nbinom") stop("Negative binomial distribution is currently only available for the identity link")
  cl <- match.call()
  #Estimating the mean structure:
  if(link=="identity") meanfit <- ingarch.fit(ts=ts, model=model, ...)
  if(link=="log") meanfit <- loglin.fit(ts=ts, model=model, ...)
  #Estimating the distribution:
  disfit <- distr.fit(meanfit, distr=distr)
  info.matrix_corrected <- if(is.null(meanfit$outerscoreprod)) NULL else apply((1/meanfit$fitted.values + disfit$sigmasq)*meanfit$outerscoreprod, c(2,3), sum)
  result <- c(
    list(coefficients=meanfit$coefficients, init=meanfit$init, residuals=meanfit$residuals, fitted.values=meanfit$fitted.values, linear.predictors=meanfit$linear.predictors, logLik=meanfit$logLik, score=meanfit$score, info.matrix=meanfit$info.matrix, info.matrix_corrected=info.matrix_corrected, call=cl, n_obs=meanfit$n_obs, ts=meanfit$ts, model=meanfit$model, link=link), #an extract of the object meanfit with an additional information matrix corrected for the fitted conditional distribution
    disfit
  )
  class(result) <- c("tsglm")
  return(result)
}
