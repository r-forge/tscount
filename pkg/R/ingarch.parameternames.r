ingarch.parameternames <- function(model){
  p <- length(model$past_obs)
  q <- length(model$past_mean)
  r <- ncol(model$xreg)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  #Set names of parameters:
  parameternames <- c(
    "(Intercept)",
    if(p>0){paste("beta", model$past_obs, sep="_")}else{NULL}, #parameters for regression on past observations
    if(q>0){paste("alpha", model$past_mean, sep="_")}else{NULL}, #parameters for regression on past means
    if(r>0){paste("eta", R, sep="_")}else{NULL} #parameters for covariates
  )
  #Use names provided with the covariates when available:
  if(!is.null(dimnames(model$xreg)[[2]])) parameternames[1+p+q+R] <- dimnames(model$xreg)[[2]]
 return(parameternames)
}
        