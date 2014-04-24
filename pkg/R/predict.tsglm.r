predict.tsglm <- function(object, n.ahead=1, newobs=NULL, newxreg=NULL, ...){
  tsglm.check(object)
  newxreg <- if(is.null(newxreg)) matrix(0, nrow=n.ahead, ncol=0) else as.matrix(newxreg)
  stopifnot(n.ahead>0,
            n.ahead%%1==0,
            ncol(newxreg)==ncol(object$model$xreg)
  )
  n <- object$n_obs
  model <- object$model
#  if(ncol(model$xreg)==0){
#    model$xreg <- cbind(model$xreg, rep(0, n))
#    model$external <- TRUE
#    object$coefficients <- c(coef(object), 0)
#  }
  p <- length(model$past_obs)
  q <- length(model$past_mean)
  r <- ncol(model$xreg)
  R <- seq(along=numeric(r)) 
  xreg <- rbind(model$xreg, newxreg)
  new <- rep(NA, n.ahead)
  new[seq(along=newobs)] <- newobs
  ts <- c(object$ts, new)
  if(is.ts(object$ts)) ts <- ts(ts, start=start(object$ts), frequency=frequency(object$ts))
  if(object$link == "log") ts <- log(ts+1) #transform observations for log-link
  kappa <- c(object$linear.predictors, rep(NA, n.ahead))
  if(is.ts(object$ts)) kappa <- ts(kappa, start=start(object$ts), frequency=frequency(object$ts))
  for(t in n+(1:n.ahead)){
    if(nrow(xreg)<t) xreg <- rbind(xreg, xreg[t-1,]) #if no current values of the covariates are given, then the values of the preceeding time point are used
    kappa[t] <- sum(coef(object)*c(1, ts[t-model$past_obs], kappa[t-model$past_mean]-(as.numeric(model$external)*coef(object)[1+p+q+R])%*%t(xreg[t-model$past_mean,]), xreg[t,]))
    if(is.na(ts[t])) ts[t] <- kappa[t]
    }
  if(is.ts(object$ts)){
    result <- window(kappa, start=tsp(object$ts)[2]+1/frequency(object$ts)) #use time series class if input time series has this class
  }else{
    result <- kappa[n+(1:n.ahead)]
  }
  if(object$link == "log") result <- exp(result) #transform predictions on original scale for log-link
  return(result)
}
