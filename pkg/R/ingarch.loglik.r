ingarch.loglik <- function(paramvec, model, ts, score=FALSE, info=c("none", "score", "hessian"), condmean=NULL, from=1){
  #Conditional log-likelihood function, score function and information matrix of an INGARCH(p,q) process (with intervention)
  #score: Logical. TRUE if score function should be computed.
  #info: Character. "none" if no information matrix should be computed, "score" for computation via first partial derivatives of the log-likelihood function, "hessian" for computation via second partial derivatives (cf. Ferland et al., 2006, section 3).
  ##############################
  #Check arguments:
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
  derivatives <- if(!score) "none" else if(info=="hessian") "second" else "first"
  condmean <- ingarch.condmean(paramvec=paramvec, model=model, ts=ts, derivatives=derivatives, condmean=condmean, from=from)
  #Load objects and remove initialisation if necessary:
  z <- condmean$z[p_max+(1:n)]
  kappa <- condmean$kappa[q_max+(1:n)]
  if(derivatives%in%c("first","second")) partial_kappa <- condmean$partial_kappa[q_max+(1:n), , drop=FALSE]
  if(derivatives=="second") partial2_kappa <- condmean$partial2_kappa[q_max+(1:n), , , drop=FALSE]    
  loglik_t <- ifelse(kappa>0, z*log(kappa)-kappa, -Inf)
  loglik <- sum(loglik_t)
  scorevec <- NULL
  if(score){
    scorevec_t <- (z/kappa-1) * partial_kappa
    scorevec <- colSums(scorevec_t)
  }
  outerscoreprod <- NULL
  infomat <- NULL
  if(info!="none"){
    if(info=="score"){
      outerscoreprod <- aperm(sapply(1:nrow(partial_kappa), function(i) partial_kappa[i,]%*%t(partial_kappa[i,]), simplify="array"), c(3,1,2))
      infomat <- apply(1/kappa*outerscoreprod, c(2,3), sum)
      #infomat <- (1/t(replicate(1+p+q+r, kappa))*t(partial_kappa)) %*% partial_kappa
    }else{
      if(info=="hessian"){
        hessian_t <- aperm((-z/kappa^2) * replicate(1+p+q+r, partial_kappa) * aperm(replicate(1+p+q+r, partial_kappa), perm=c(1,3,2)), perm=c(2,3,1)) + rep((z/kappa-1), each=(1+p+q+r)^2) * aperm(partial2_kappa, perm=c(2,3,1))
        infomat <- -apply(hessian_t, c(1,2), sum)
      }} 
  }
  result <- list(loglik=loglik, score=scorevec, info=infomat, outerscoreprod=outerscoreprod, kappa=kappa)
  return(result)
}
 