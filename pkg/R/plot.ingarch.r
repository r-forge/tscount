plot.ingarch <- function(x, ask=TRUE, ...){
  op <- par(ask=ask)
  acf(x$residuals, main="ACF of the residuals")
  hist(x$residuals, main="Histogram of the residuals", xlab="Residuals")
  plot(1:length(x$ts), x$residuals, xlab="Time", ylab="Residuals", main="Residuals over time")
  plot(x$fitted.values, x$residuals,xlab="Fitted values", ylab="Residuals", main="Residuals vs. Fitted")
  par(op)
  invisible()
}
