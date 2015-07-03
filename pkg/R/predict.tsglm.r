predict.tsglm <- function(object, n.ahead=1, newobs=NULL, newxreg=NULL, level=0.95, global=FALSE, type=c("quantiles", "shortest"), method=c("conddistr", "bootstrap"), B=1000, ...){
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
  stopifnot(
    length(level)==1,
    !is.na(level),
    level<1,
    level>=0
  )
  if(level>0){ #do not compute prediction intervals for level==0
    level_local <- if(global){1-(1-level)/n.ahead}else{level} #Bonferroni adjustment of the coverage rate of each individual prediction interval such that a global coverage rate is achieved
    type <- match.arg(type)
    method <- match.arg(method, several.ok=TRUE)
    conddistr_possible <- n.ahead==1 || (length(newobs)>=n.ahead-1 && !any(is.na(newobs[1:(n.ahead-1)]))) #method="conddistr" is only possible if all predictions are 1-step ahead predictions such that the true previous observations are available (this is the case if only one observation is to be predicted or when the first n.ahead-1 observations are given in argument 'newobs')
    if(all(c("conddistr", "bootstrap") %in% method)) method <- if(conddistr_possible){"conddistr"}else{"bootstrap"} #automatic choice of the method, prefer method="conddistr" if possible 
    if(type=="quantiles") a <- (1-level_local)/2
    if(type=="shortest"){
      largestdensityinterval <- function(probs, level){ #find shortest interval with given probability, probs are assumed to be the probabilities corresponding to the values seq(along=probs)-1 
        values <- seq(along=probs)-1
        ord <- order(probs, decreasing=TRUE)
        result <- range(values[ord][1:which(cumsum(probs[ord])>=level)[1]])
        return(result)
      }
    }
    if(method=="conddistr"){
      if(!conddistr_possible) stop("Computation of prediction intervals with argument 'method' set to \"bootstrap\"\ndoes only work for 1-step-ahead predictions. If argument 'n.ahead' is larger\nthan 1, future observations have to be provided in argument 'newobs'.") 
      if(type=="quantiles"){  
        qdistr_mod <- function(p, meanvalue, distr=c("poisson", "nbinom"), distrcoefs, upper=FALSE){ #allows for alternative definition of the quantile
          result <- qdistr(p=p, meanvalue=meanvalue, distr=distr, distrcoefs=distrcoefs)
          if(upper) result <- result + (pdistr(q=result, meanvalue=meanvalue, distr=distr, distrcoefs=distrcoefs)==p) #alternative definition of the quantile
          return(result)
        }
        predint <- cbind(lower=qdistr_mod(a, meanvalue=pred, distr=object$distr, distrcoefs=object$distrcoefs), upper=qdistr_mod(1-a, meanvalue=pred, distr=object$distr, distrcoefs=object$distrcoefs, upper=TRUE))
      }
      if(type=="shortest"){
        cutoff <- qdistr(p=1-(1-level_local)/10, meanvalue=max(pred), distr=object$distr, distrcoefs=object$distrcoefs) #very large quantile which is almost certainly larger than the upper bound of the shortest prediction interval
        predint <- t(sapply(pred, function(predval) largestdensityinterval(probs=ddistr(x=0:cutoff, meanvalue=predval, distr=object$distr, distrcoefs=object$distrcoefs), level=level_local)))
      }
      predmed <- qdistr(p=0.5, meanvalue=pred, distr=object$distr, distrcoefs=object$distrcoefs)
      B <- NULL
    }
    if(method=="bootstrap"){
      if(!is.null(newobs)) stop("Computation of prediction intervals with argument 'method' set to \"bootstrap\"\ncan currently not take into account the future observations given in argument\n'newobs'. Either set argument 'method' to \"conddistr\" or do not compute\nprediction intervals at all by setting argument 'level' to zero.")
      stopifnot(
        length(B)==1,
        B%%1==0,
        B>=10
      )
      futureobs <- replicate(B, tsglm.sim(n=n.ahead, xreg=xreg[-(1:n), , drop=FALSE], fit=object, n_start=0)$ts)
      if(type=="quantiles") predint <- t(apply(futureobs, 1, quantile, probs=c(a, 1-a), type=1)) 
      if(type=="shortest") predint <- t(apply(futureobs, 1, function(x) largestdensityinterval(tabulate(x+1)/length(x), level=level_local)))
      predmed <- apply(futureobs, 1, median)
    }
    colnames(predint) <- c("lower", "upper")
    if(is.ts(object$ts)){ #use time series class if input time series has this class
        predint <- ts(predint, start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts))
        predmed <- ts(predmed, start=tsp(object$ts)[2]+1/frequency(object$ts), frequency=frequency(object$ts))
    }
    result <- c(result, list(interval=predint, level=level, global=global, type=type, method=method, B=B, median=predmed))
  }
  return(result)
}
