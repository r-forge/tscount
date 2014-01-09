ingarch.var <- function(intercept, past_obs, past_mean){
#Theoretical marginal variance of an INGARCH(p,q) process
##############################
  result <- ingarch.acf(intercept=intercept, past_obs=past_obs, past_mean=past_mean, lag.max=0, type="acvf", plot=FALSE)[[1]]
  return(result)
}
