summary.tsglm <- function(object, B, parallel=FALSE, ...){
  cl <- object$call
  if(length(coef(object))){
    ll <- logLik(object) #log-likelihood of the fitted model
    k <- attr(ll, "df") #number of parameters of the fitted model
    n <- attr(ll, "nobs") #number of observations used for model fit
    infer <- NULL
    try(infer <- se(object, B=B, parallel=parallel))  
    if(is.null(object$info.matrix_corrected) | is.null(infer)){
      coefs <- data.frame(Estimate=c(coef(object), object$distrcoefs))
    }else{
      coefs <- data.frame(
        infer$est,
        infer$se
      )
      names(coefs) <- c("Estimate", "Std. Error")
    }
    result <- list(call=cl,
      link=object$link,
      distr=object$distr,
      residuals=residuals(object, type="response"),
      coefficients=coefs,
      number.coef=nrow(coefs),             
      logLik=logLik(object),             
      AIC=AIC(object), #Akaike's Information Criterion
      AICc= -2*ll + 2*k + 2*k*(k+1)/(n-k-1), #AIC corrected for finite sample sizes
      BIC=BIC(object), #Bayesian Information Criterion
      pearson.resid=residuals(object, type="pearson") #Pearson's residuals
    )  
  }else{ 
    result <- list(call=cl, init=object$init)
  }    
  class(result) <- "summary.tsglm"
  return(result)
}
