ingarch.condmean <- function(paramvec, model, ts, derivatives=c("none", "first", "second"), condmean=NULL, from=1){
  #Recursion for the conditional mean and its derivatives of an INGARCH(p,q) process (with intervention)
  #derivatives. Character. "none" does only the recursion for the conditional mean, "first" additionally computes first partial derivatives, "second" also the second partial derivatives.
  #condmean: List. Output of a previous call of this function with all arguments except tau identical to this call and tau of this call <= tau of the previous call. The recursion up to tau is taken from condmean, so that only the recursion from time point tau on has to be computed. If NULL the complete recursion is computed. For not computing anything of the recursion set argument tau=Inf.
  #############################
  #Check arguments:
  n <- length(ts)
  p <- length(model$past_obs)
  P <- seq(along=numeric(p)) #sequence 1:p if p>0 and NULL otherwise
  p_max <- max(model$past_obs, 0)
  q <- length(model$past_mean)
  Q <- seq(along=numeric(q)) #sequence 1:q if q>0 and NULL otherwise
  q_max <- max(model$past_mean, 0)
  Q_max <- seq(along=numeric(q_max))
  r <- max(ncol(model$xreg), 0)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  parameternames <- tsglm.parameternames(model)
  derivatives <- match.arg(derivatives)
  param <- list( #transform parameter vector to a list
    intercept=paramvec[1],
    past_obs=paramvec[1+P],
    past_mean=paramvec[1+p+Q],
    xreg=paramvec[1+p+q+R]
  )    
  if(!is.null(condmean)){ #If the output of a previous call is provided, the recursion starts from t=from. Else initialisation of all objects is necessary and the recursion starts from t=1.
    times <- if(from <= n) from:n else NULL
    #Load objects:
    z <- condmean$z
    kappa <- condmean$kappa
    if(derivatives %in% c("first","second")) partial_kappa <- condmean$partial_kappa
    if(derivatives == "second") partial2_kappa <- condmean$partial2_kappa
#########include checks if argument condmean is sufficient for further calculations
  }else{  
    times <- 1:n
    #Initialisation by stationary solution (and its partial derivatives):
    denom <- 1-sum(param$past_obs)-sum(param$past_mean)    
    kappa_stationary <- param$intercept/denom
    kappa <- c(rep(kappa_stationary, q_max), numeric(n))  
    z <- c(as.integer(rep(round(kappa_stationary), p_max)), ts)
    if(derivatives %in% c("first", "second")){
      #Vector of first partial derivatives of kappa with respect to the parameters:
      partial_kappa <- matrix(0, nrow=n+q_max, ncol=1+p+q+r)
      partial_kappa[Q_max, 1] <- 1/denom #intercept
      partial_kappa[Q_max, 1+P] <- param$intercept/denom^2 #past_obs
      partial_kappa[Q_max, 1+p+Q] <- param$intercept/denom^2 #past_mean
      #derivatives with respect to the regressor coefficients are zero (which is the default)
      if(derivatives == "second"){
        #Matrix of second partial derivatives of kappa with respect to the parameters:
        partial2_kappa <- array(0, dim=c(n+q_max, 1+p+q+r, 1+p+q+r))  
        partial2_kappa[Q_max, 1, 1+c(P,p+Q)] <- partial2_kappa[Q_max, 1+c(P,p+Q), 1] <- 1/denom^2
        partial2_kappa[Q_max, 1+c(P,p+Q), 1+c(P,p+Q)] <- 2*param$intercept/denom^3
        #derivatives with respect to the regressor coefficients are zero
      }
    }
  }
  X <- matrix(0, nrow=q_max+n, ncol=r)
  X[q_max+(1:n), ] <- model$xreg
  for(t in times){
    kappa[t+q_max] <- param$intercept + sum(param$past_obs*z[(t-model$past_obs)+p_max]) + sum(param$past_mean*kappa[(t-model$past_mean)+q_max]) + if(r>0){sum(param$xreg*X[t+q_max, ]) - if(q>0){sum(param$past_mean*colSums(model$external*param$xreg*t(X[(t-model$past_mean)+q_max, , drop=FALSE])))}else{0}}else{0}   
  }
  result <- list(z=z, kappa=kappa)    
  if(derivatives %in% c("first", "second")){
    for(t in times){
      partial_kappa[t+q_max, 1] <- 1 + sum(param$past_mean*partial_kappa[(t-model$past_mean)+q_max, 1]) #intercept
      if(p>0) partial_kappa[t+q_max, 1+P] <- z[t-model$past_obs+p_max] + (if(q>0){t(param$past_mean) %*% partial_kappa[(t-model$past_mean)+q_max, 1+P, drop=FALSE]}else{numeric(p)}) #past_obs    
      if(q>0) partial_kappa[t+q_max, 1+p+Q] <- kappa[t-model$past_mean+q_max] + t(param$past_mean) %*% partial_kappa[(t-model$past_mean)+q_max, 1+p+Q, drop=FALSE] - (if(r>0){param$past_mean*colSums(model$external*param$xreg*t(X[(t-model$past_mean)+q_max, , drop=FALSE]))}else{numeric(q)}) #past_mean
      if(r>0) partial_kappa[t+q_max, 1+p+q+R] <- (if(q>0){colSums(param$past_mean*partial_kappa[(t-model$past_mean)+q_max, 1+p+q+R, drop=FALSE]) - model$external*colSums(param$past_mean*X[(t-model$past_mean)+q_max, , drop=FALSE])}else{numeric(r)}) + X[t+q_max, ] #covariates
    }
    dimnames(partial_kappa)[[2]] <- if(p==0 & q==0) list(parameternames) else parameternames
    result <- c(result, list(partial_kappa=partial_kappa))
    if(derivatives == "second"){
      for(t in times){
        partial2_kappa[t+q_max, , ] <- apply(param$past_mean*partial2_kappa[t+q_max-model$past_mean, , , drop=FALSE], c(2,3), sum)
        partial2_kappa[t+q_max, 1+p+Q, 1+p+Q] <- partial2_kappa[t+q_max, 1+p+Q, 1+p+Q] + (partial_kappa[t+q_max-model$past_mean, 1+p+Q] + t(partial_kappa[t+q_max-model$past_mean, 1+p+Q]))/2 #from the formula we would only need the first part of the last summand, but in this case our matrix is not symmetrical, so we add this average
        partial2_kappa[t+q_max, 1+p+Q, 1+p+q+R] <- partial2_kappa[t+q_max, 1+p+Q, 1+p+q+R] + partial_kappa[t+q_max-model$past_mean, 1+p+q+R] - X[t+q_max-model$past_mean,]
        partial2_kappa[t+q_max, 1+p+q+R, 1+p+Q] <- t(partial2_kappa[t+q_max, 1+p+Q, 1+p+q+R])
      }
      dimnames(partial_kappa)[[2]] <- dimnames(partial2_kappa)[[3]] <- if(p==0 & q==0) list(parameternames) else parameternames
      result <- c(result, list(partial2_kappa=partial2_kappa))  
    }
  }
  return(result)
} 
