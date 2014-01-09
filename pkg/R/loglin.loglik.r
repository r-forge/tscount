loglin.loglik <- function(paramvec, model, ts, score=FALSE, info=c("none", "score"), condmean=NULL, from=1){
  #Conditional log-likelihood function, score function and information matrix of a log-linear autoregressive process (with intervention)
  #score: Logical. TRUE if score function should be computed.
  #info: Character. "none" if no information matrix should be computed, "score" for computation via first partial derivatives of the log-likelihood function.

  ##############
  #Checks and preparations:
  n <- length(ts)
  p <- length(model$past_obs)
  p_max <- max(model$past_obs, 0)
  q <- length(model$past_mean)
  q_max <- max(model$past_mean, 0)
  r <- max(ncol(model$xreg), 0)
  R <- seq(along=numeric(r)) #sequence 1:r if r>0 and NULL otherwise
  info <- match.arg(info)
  if(!score & info!="none"){
    score <- TRUE
    warning("Information matrix cannot be calculated without score vector. Argument score is set to TRUE.")
  }
  derivatives <- if(!score) "none" else "first"
  ##############
  
  condmean <- loglin.condmean(paramvec=paramvec, model=model, ts=ts, derivatives=derivatives, condmean=condmean, from=from)
  #Load objects and remove initialisation if necessary:
  z <- condmean$z[p_max+(1:n)]
  kappa <- condmean$kappa[q_max+(1:n)]
  if(derivatives == "first") partial_kappa <- condmean$partial_kappa[q_max+(1:n), , drop=FALSE]
  loglik_t <- z*kappa-exp(kappa)
  loglik <- sum(loglik_t)
  scorevec <- NULL
  if(score){
    scorevec_t <- (z-exp(kappa)) * partial_kappa
    scorevec <- colSums(scorevec_t)
  }
  outerscoreprod <- NULL
  infomat <- NULL
  if(info!="none"){
    outerscoreprod <- if(p+q+r > 0) aperm(sapply(1:n, function(i) partial_kappa[i,]%*%t(partial_kappa[i,]), simplify="array"), c(3,1,2)) else array(partial_kappa[,1]^2, dim=c(n,1,1))
      infomat <- apply(exp(kappa)*outerscoreprod, c(2,3), sum)
  }
  result <- list(loglik=loglik, score=scorevec, info=infomat, outerscoreprod=outerscoreprod, kappa=kappa)
  return(result)
}
 