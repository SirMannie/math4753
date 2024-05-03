#' @title bootstrap function
#'
#' @param iter Number of bootstrap iterations (default: 10000)
#' @param x Numeric vector of data points
#' @param fun Function to calculate the statistic (default: mean)
#' @param alpha Significance level for confidence interval (default: 0.05)
#' @param cx Control parameter for text size in plots.
#' @param ... Additional arguments passed to other functions, such as graphical parameters for `hist`.
#'
#' @return A list containing the confidence interval, statistic function, original data, and point estimate
#' @export
#'
#' @examples
#' \dontrun{data <- rnorm(100) result <- myboot2(x=data, iter=1000, fun=mean)}
#'
#' @importFrom stats quantile
#' @importFrom graphics abline segments text




myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
