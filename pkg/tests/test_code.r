set.seed(2014)

n_1 <- 30
n_start <- 5
intervent_1 <- interv_covariate(n=n_1, tau=20, delta=0.8)
model_1 <- list(past_obs=1, past_mean=1, xreg=intervent_1, external=FALSE)
param_1 <- list(intercept=2, past_obs=0.3, past_mean=0.2, xreg=3)
sim <- ingarch.sim(n=n_1, param=param_1, model=model_1, n_start=10)
ingarch(model=model_1, ts=sim$ts, score=TRUE, info="hessian")
ingarch(model=model_1, ts=sim$ts, score=TRUE, info="none")
ingarch(model=model_1, ts=sim$ts, score=FALSE, info="none")
test <- ingarch(model=model_1, ts=sim$ts)
summary(test)
plot(test)
residuals(test)
plot(test)
logLik(test)
AIC(test)
ingarch.se(test)
predict(test, n.ahead=4)

#Functions for analytical mean, variance and autocorrelation:
ingarch.mean(0.3, c(0.1,0.1), 0.1)
ingarch.acf(0.3, c(0.1,0.1,0.1), 0.1, type="acf", lag.max=15)
ingarch.var(0.3, c(0.1,0.1), 0.1)

#Functions for intervention detection:
intes <- interv_test(fit=test, tau=20, delta=0.8, est_interv=TRUE, external=TRUE)
intes2 <- interv_detect(fit=test, tau=10:15, delta=1, est_interv=TRUE, B=2)
plot(intes2)
intes3 <- interv_multiple(fit=test, tau=10:11, delta=1, B=2, signif_level=0.2)
plot(intes3)
