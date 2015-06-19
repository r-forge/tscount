predict.tsglm <- function(object, n.ahead=1, newobs=NULL, newxreg=NULL, level=0.95, B, ...){
  tsglm.check(object)
  #Link and related functions:
  if(object$link=="identity"){
    trafo <- function(x) x #transformation function
    g_inv <- function(x) x  #inverse of link function
  }
  if(object$link=="log"){
    trafo <- function(x) if(!is.null(x)) log(x+1) else NULL #transformation function
    g_inv <- function(x) exp(x) #inverse of link function
  }
  newxreg <- if(is.null(newxreg)) matrix(0, nrow=n.ahead, ncol=ncol(object$xreg)) else as.matrix(newxreg)  #if no covariates are provided, these are set to zero
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
  if(nrow(xreg) < n+n.ahead) xreg <- rbind(xreg, matrix(xreg[nrow(xreg), ], nrow=n+n.ahead-nrow(xreg), ncol=r, byrow=TRUE)) #if not enough future values of the covariates are given, then the values of the last available time point are used, which is usually NOT sensible!
  new <- rep(NA, n.ahead)
  new[seq(along=newobs)] <- newobs
  ts <- c(object$ts, new)
  if(is.ts(object$ts)) ts <- ts(ts, start=start(object$ts), frequency=frequency(object$ts))
  kappa <- c(rep(NA, n-object$n_eff), object$linear.predictors, rep(NA, n.ahead))
  if(is.ts(object$ts)) kappa <- ts(kappa, start=start(object$ts), frequency=frequency(object$ts))
  for(t in n+(1:n.ahead)){
    kappa[t] <- sum(coef(object)*c(1, trafo(ts[t-model$past_obs]), kappa[t-model$past_mean]-if(r>0){sum((as.numeric(model$external)*coef(object)[1+p+q+R])*t(xreg[t-model$past_mean,]))}else{0}, xreg[t,])) 
    if(is.na(ts[t])) ts[t] <- g_inv(kappa[t]) #unobserved future observations are replaced by their prediction (by the conditional mean)
  }
  if(is.ts(object$ts)){
    pred <- window(g_inv(kappa), start=tsp(object$ts)[2]+1/frequency(object$ts)) #use time series class if input time series has this class
  }else{
    pred <- g_inv(kappa)[n+(1:n.ahead)]
  }
  result <- list(pred=pred)

  #Prediction intervals:
  if(!is.null(newobs) && !missing(B)){
    warning("Prediction intervals are only available for a repeated 1-step ahead\nprediction where either all (prediction intervals of type 'condquant')\nor no (prediction intervals of type 'bootstrap') future observations\nare given in argument 'newobs'. It is currently not possible to compute\nprediction intervals when only some of the future observations are\navailable (except when only the lastfuture observation is missing).")
    return(result)
  }
  a <- (1-level)/2
  if(n.ahead==1 || !any(is.na(newobs[1:(n.ahead-1)]))){ #all predictions are 1-step ahead predictions, type="condquant"
    #Quantile function of the conditional distribution (cf. marcal.gslm):
      if(object$distr=="poisson"){
        qdistr <- function(p, meanvalue, distrcoefs, upper=FALSE){
          quant <- qpois(p, lambda=meanvalue)
          if(upper && p==ppois(quant, lambda=meanvalue)) quant <- quant+1 #different definition of the quantile
          return(quant)
        }
      }
      if(object$distr=="nbinom"){
        qdistr <- function(p, meanvalue, distrcoefs, upper=FALSE){
          quant <- qnbinom(p, mu=meanvalue, size=distrcoefs[["size"]])
          if(upper && p==pnbinom(quant, mu=meanvalue, size=distrcoefs[["size"]])) quant <- quant+1 #different definition of the quantile
          return(quant)
        }
      }
    predint_condquant <- cbind(lower=qdistr(a, meanvalue=pred, distrcoefs=object$distrcoefs), upper=qdistr(1-a, meanvalue=pred, distrcoefs=object$distrcoefs, upper=TRUE))
    if(is.ts(object$ts)){ #use time series class if input time series has this class
      predint_condquant <- ts(predint_condquant, start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts))
    }
    result <- c(result, list(interval_condquant=predint_condquant, type="condquant"))          
  }
  if(!missing(B)){ #type="bootstrap"
    futureobs <- replicate(B, tsglm.sim(n=n.ahead, xreg=xreg[-(1:n), , drop=FALSE], fit=object, n_start=0)$ts)
    pred_median <- apply(futureobs, 1, median)    
    largestdensityinterval <- function(x, level){ #find shortest interval with given probability 
      probs <- tabulate(x+1)/length(x)
      ord <- order(probs, decreasing=TRUE)
      result <- range((seq(along=probs)-1)[ord][1:which(cumsum(probs[ord])>=level)[1]])
      return(result)
    }
    predint_shortest <- t(apply(futureobs, 1, largestdensityinterval, level=level))
    predint_quantiles <- t(apply(futureobs, 1, quantile, probs=c(a, 1-a), type=8))
    colnames(predint_shortest) <- colnames(predint_quantiles) <- c("lower", "upper")
    if(is.ts(object$ts)){ #use time series class if input time series has this class
      pred_median <- ts(pred_median, start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts)) 
      predint_shortest <- ts(predint_shortest, start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts))
      predint_quantiles <- ts(predint_quantiles, start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts))  
    }
    result <- c(result, list(median=pred_median, interval_shortest=predint_shortest, interval_quantiles=predint_quantiles, B=B, type="bootstrap")) 
  }
  return(result)
}
