interv_test <- function(...) UseMethod("interv_test")

interv_test.ingarch <- function(fit, tau, delta, external, info=c("score", "hessian"), est_interv=FALSE, ...){
#Test on one or several interventions of known types at known points in time
##############################
  
  #Check and modify argument:  
  ingarch.check(fit)
  info <- match.arg(info)
  r <- length(tau)
  if(missing(external) | length(external)==0) external <- rep(FALSE, r) else external <- as.logical(external) #the default value for external is FALSE (i.e. an internal intervention effect)
  if(length(external)==1) external <-  rep(external, r) else external <- as.logical(external) #if only one value for external is provided, this is used for all interventions
  
  #Add information about intervention effects:
    param_H0_extended <- c(fit$coefficients, numeric(r))
    model_extended <- fit$model
    covariate <- interv_covariate(n=length(fit$ts), tau=tau, delta=delta)
    if(r > 0){colnames(covariate) <- {paste("eta", 1:r, sep="_")}}
    model_extended$xreg <- cbind(fit$model$xreg, covariate)
    model_extended$external <- c(fit$model$external, external) 
  loglik <- ingarch.loglik(paramvec=param_H0_extended, model=model_extended, ts=fit$ts, score=TRUE, info=info)
  infomat_corrected <- apply((1/loglik$kappa + fit$sigmasq)*loglik$outerscoreprod, c(2,3), sum)
  vcov <- vcov.ingarch(list(info.matrix=loglik$info, info.matrix_corrected=infomat_corrected))
  test_statistic <- (t(loglik$score) %*% vcov %*% loglik$score)[1,1]
  p_value <- 1-pchisq(test_statistic, df=r)
  result <- list(
    test_statistic=test_statistic,
    df=r,
    p_value=p_value,
    fit_H0=fit,
    model_interv=model_extended
  )
  if(est_interv){ #ML estimation for the model with intervention
      fit_interv <- ingarch(ts=fit$ts, model=model_extended, ...)
      result <- c(result, list(fit_interv=fit_interv))  
  }
  class(result) <- "interv_test"
  return(result)
}
