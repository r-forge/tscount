simparam <- function(...) UseMethod("simparam")

simparam.tsglm <- function(object, n=1){
  rmvnorm_stable <- function(n, mean=rep(0, nrow(sigma)), sigma=diag(length(mean))){
    #Function for stable generation of random values from a multivariate normal distribution (is robust against numerical deviations from symmetry of the covariance matrix. Code is taken from function rmvnorm in the package mvtnorm and modified accordingly.
    if(length(mean) != nrow(sigma)) stop("mean and sigma have non-conforming size")
    ev <- eigen(sigma, symmetric=TRUE)
    ev$values[ev$values < 0] <- 0
    R <- t(ev$vectors %*% (t(ev$vectors) * sqrt(ev$values)))
    centred <- matrix(rnorm(n=n*ncol(sigma)), nrow=n, byrow=TRUE) %*% R
    result <- sweep(centred, 2, mean, "+")
    colnames(result) <- names(mean)
    return(result)
  }
  f <- 1.1 #one could choose this factor according to the probability of a parameter from the multivariate normal distribution to be outside the parameter space
  param <- rmvnorm_stable(n=ceiling(f*n), mean=coef(object), sigma=vcov(object))
  repeat{
    valid_param <- apply(param, 1, function(x) tsglm.parametercheck(tsglm.parameterlist(paramvec=x, model=object$model), link=object$link, stopOnError=FALSE))
    if(sum(valid_param) >= n) break
    param <- rbind(param, rmvnorm_stable(n=ceiling((n-sum(valid_param))*f/mean(valid_param)), mean=coef(object), sigma=vcov(object)))
  }
  use_param <- which(valid_param)[1:n]
  param <- param[use_param, ]
  n_invalid <- max(use_param) - n
  result <- list(param=param, n_invalid=n_invalid)
  return(result)
}        
