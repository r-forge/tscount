plot.tsglm <- function(x, ask=TRUE, ...){
  devAskNewPage(ask=ask)
  residu <- residuals(x, type="pearson")
  acf(residu, main="ACF of Pearson residuals")
  #hist(residu, main="Histogram of Pearson residuals", xlab="Residuals")
  plot(residu, type="o", xlab="Time", ylab="Residuals", main="Pearson residuals over time")
  if(require(MASS)){
    par(ann=FALSE)
    cpgram(residu, main="")
    title(main="Cumulative periodogram of Pearson residuals", xlab="Frequency")
    par(ann=TRUE)
  }else{
    warning("The plot of the cumulative periodogram of the residuals is omitted, because it\ndepends on the package 'MASS', which is not installed.")
  }
  pit(x)
  marcal(x)
  invisible()
}
