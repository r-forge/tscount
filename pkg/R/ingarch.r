ingarch <- function(ts, model=list(past_obs=NULL, past_mean=NULL, xreg=NULL, external=NULL), distr=c("poisson", "nbinom"), ...){
  distr <- match.arg(distr)
  cl <- match.call()
  #Estimating the mean structure:
  ingfit <- ingarch.fit(ts=ts, model=model, ...)
  #Estimating the distribution:
  disfit <- distr.fit(ingfit, distr=distr)
  info.matrix_corrected <- if(is.null(ingfit$outerscoreprod)) NULL else apply((1/ingfit$fitted.values + disfit$sigmasq)*ingfit$outerscoreprod, c(2,3), sum)
  result <- c(
    list(coefficients=ingfit$coefficients, init=ingfit$init, residuals=ingfit$residuals, fitted.values=ingfit$fitted.values, linear.predictors=ingfit$linear.predictors, logLik=ingfit$logLik, score=ingfit$score, info.matrix=ingfit$info.matrix, info.matrix_corrected=info.matrix_corrected, call=cl, n_obs=ingfit$n_obs, ts=ingfit$ts, model=ingfit$model), #an extract of the object ingfit with an additional information matrix corrected for the fitted conditional distribution
    disfit
  )
  class(result) <- c("ingarch", "tsglm")
  return(result)
}
