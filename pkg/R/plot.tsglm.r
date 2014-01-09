plot.tsglm <- function(x, ask=TRUE, ...){
  op <- par(ask=ask)
  residu <- residuals(x, type="pearson")
  acf(residu, main="ACF of Pearson residuals")
  hist(residu, main="Histogram of Pearson residuals", xlab="Residuals")
  plot(1:length(x$ts), residu, xlab="Time", ylab="Residuals", main="Pearson Residuals over time")
  if(require(MASS)) cpgram(residu, main="Cumulative periodogram of Pearson residuals")
  par(op)
  invisible()
}
