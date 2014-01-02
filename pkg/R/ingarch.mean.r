ingarch.mean <- function(intercept, past_obs, past_mean){
#Theoretical marginal mean of an INGARCH(p,q) process without interventions
##############################
  result <- (intercept/(1-sum(past_mean)-sum(past_obs)))[[1]]
  return(result)
}
