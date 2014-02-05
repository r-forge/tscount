ingarch.loglik <- function(paramvec, model, ts, score=FALSE, info=c("none", "score", "hessian", "both"), condmean=NULL, from=1, init=c("marginal", "zero", "firstobs")){
  #Conditional (quasi) log-likelihood function, score function and information matrix of a count time series following generalised linear models
  
  ##############
  #Checks and preparations:
  init <- match.arg(init)
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
  derivatives <- if(!score) "none" else if(info %in% c("hessian", "both")) "second" else "first"
  parameternames <- tsglm.parameternames(model)
  ##############
  
  condmean <- ingarch.condmean(paramvec=paramvec, model=model, ts=ts, derivatives=derivatives, condmean=condmean, from=from, init=init)
  #Load objects and remove initialisation if necessary:
  z <- condmean$z[p_max+(1:n)]
  kappa <- condmean$kappa[q_max+(1:n)]
  if(derivatives %in% c("first", "second")) partial_kappa <- condmean$partial_kappa[q_max+(1:n), , drop=FALSE]
  if(derivatives == "second") partial2_kappa <- condmean$partial2_kappa[q_max+(1:n), , , drop=FALSE]   
  loglik_t <- ifelse(kappa>0, z*log(kappa)-kappa, -Inf)
  loglik <- sum(loglik_t)
  scorevec <- NULL
  if(score){
    scorevec_t <- (z/kappa-1) * partial_kappa
    scorevec <- colSums(scorevec_t)
  }
  outerscoreprod <- NULL
  infomat <- NULL
  if(info != "none"){
    if(info %in% c("score", "both")){
      outerscoreprod <- array(NA, dim=c(n, 1+p+q+r, 1+p+q+r), dimnames=list(NULL, parameternames, parameternames))
      outerscoreprod[] <- if(p+q+r > 0) aperm(sapply(1:n, function(i) partial_kappa[i,]%*%t(partial_kappa[i,]), simplify="array"), c(3,1,2)) else array(partial_kappa[,1]^2, dim=c(n,1,1))
      infomat <- infomat_score <- apply(1/kappa*outerscoreprod, c(2,3), sum)
    }
    if(info %in% c("hessian", "both")){
      hessian_t <- aperm((-z/kappa^2) * replicate(1+p+q+r, partial_kappa) * aperm(replicate(1+p+q+r, partial_kappa), perm=c(1,3,2)), perm=c(2,3,1)) + rep((z/kappa-1), each=(1+p+q+r)^2) * aperm(partial2_kappa, perm=c(2,3,1))
      infomat <- infomat_hessian <- -apply(hessian_t, c(1,2), sum)
    }
    if(info == "both"){
      infomat <- infomat_hessian %*% invertinfo(infomat_score, stopOnError=TRUE)$vcov %*% infomat_hessian
      outerscoreprod <- NULL
    }
    dimnames(infomat) <- list(parameternames, parameternames) 
  }
  result <- list(loglik=loglik, score=scorevec, info=infomat, outerscoreprod=outerscoreprod, kappa=kappa)
  return(result)
}
