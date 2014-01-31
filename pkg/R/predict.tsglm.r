predict.tsglm <- function(object, n.ahead=1, newobs=NULL, newxreg=NULL, ...){
  tsglm.check(object)
  stopifnot(n.ahead>0,
            n.ahead%%1==0,
            is.null(newxreg)||ncol(newxreg)==ncol(object$model$xreg)||length(newxreg)==ncol(object$model$xreg))
  n <- object$n_obs
  model <- object$model
  if(ncol(model$xreg)==0){
    model$xreg <- cbind(model$xreg, rep(0, n))
    model$external <- TRUE
    object$coefficients <- c(coef(object), 0)
  }
  p <- length(model$past_obs)
  q <- length(model$past_mean)
  r <- ncol(model$xreg)
  R <- seq(along=numeric(r)) 
  xreg <- rbind(model$xreg, newxreg)
  new <- rep(NA, n.ahead)
  new[seq(along=newobs)] <- newobs
  ts <- c(object$ts, new)
  if(object$link == "log") ts <- log(ts+1) #transform observations for log-link
  kappa <- object$linear.predictors
  for(t in n+(1:n.ahead)){
    if(nrow(xreg)<t) xreg <- rbind(xreg, xreg[t-1,])
    kappa[t] <- sum(coef(object)*c(1, ts[t-model$past_obs], kappa[t-model$past_mean]-(as.numeric(model$external)*coef(object)[1+p+q+R])%*%t(xreg[t-model$past_mean,]), xreg[t,]))
    if(is.na(ts[t])) ts[t] <- kappa[t]
    }
  names(kappa) <- 1:(n+n.ahead)
  result <- kappa[n+(1:n.ahead)]
  if(object$link == "log") result <- exp(result) #transform predictions on original scale for log-link
  return(result)
}