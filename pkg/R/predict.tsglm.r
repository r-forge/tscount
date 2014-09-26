predict.tsglm <- function(object, n.ahead=1, newobs=NULL, newxreg=NULL, level=0.95, B, ...){
  tsglm.check(object)
  newxreg <- if(is.null(newxreg)) matrix(0, nrow=n.ahead, ncol=ncol(object$xreg)) else as.matrix(newxreg) #if no covariates are provided, these are set to zero
  stopifnot(n.ahead>0,
            n.ahead%%1==0,
            ncol(newxreg)==ncol(object$xreg)
  )
  n <- object$n_obs
  model <- object$model
  p <- length(model$past_obs)
  q <- length(model$past_mean)
  r <- ncol(object$xreg)
  R <- seq(along=numeric(r)) 
  xreg <- rbind(object$xreg, newxreg)
  if(nrow(xreg) < n+n.ahead) xreg <- rbind(xreg, t(replicate(n+n.ahead-nrow(xreg), xreg[nrow(xreg), ]))) #if not enough future values of the covariates are given, then the values of the last available time point are used
  new <- rep(NA, n.ahead)
  new[seq(along=newobs)] <- newobs
  ts <- c(object$ts, new)
  if(is.ts(object$ts)) ts <- ts(ts, start=start(object$ts), frequency=frequency(object$ts))
  if(object$link == "log") ts <- log(ts+1) #transform observations for log-link
  kappa <- c(object$linear.predictors, rep(NA, n.ahead))
  if(is.ts(object$ts)) kappa <- ts(kappa, start=start(object$ts), frequency=frequency(object$ts))
  for(t in n+(1:n.ahead)){
    kappa[t] <- sum(coef(object)*c(1, ts[t-model$past_obs], kappa[t-model$past_mean]-if(r>0){(as.numeric(model$external)*coef(object)[1+p+q+R])%*%t(xreg[t-model$past_mean,])}else{0}, xreg[t,])) 
    if(is.na(ts[t])) ts[t] <- kappa[t] #unobserved future observations are replaced by their prediction (by the conditional mean)
    }
  if(is.ts(object$ts)){
    pred <- window(kappa, start=tsp(object$ts)[2]+1/frequency(object$ts)) #use time series class if input time series has this class
  }else{
    pred <- kappa[n+(1:n.ahead)]
  }
  if(object$link == "log") pred <- exp(pred) #transform predictions on original scale for log-link
  result <- list(pred=pred)

  #Prediction intervals:
  if(!missing(B)){
    futureobs <- t(replicate(B, tsglm.sim(n=n.ahead, xreg=xreg[-(1:n), , drop=FALSE], fit=object, n_start=0)$ts))
    predint <- apply(futureobs, 2, quantile, probs=c((1-level)/2, 1-(1-level)/2))
    rownames(predint) <- c("lower", "upper")
    if(is.ts(object$ts)){
      predint <- ts(t(predint), start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts)) #use time series class if input time series has this class
    }
    result <- c(result, list(interval=predint, type="bootstrap", B=B)) 
  }
  return(result)
}
