
#' Compute error statistics of load estimates
#'
#' @param x data frame containing observed, predicted and residual loads
#' @param n.coeff number of coefficients used to make predictions (for computing degrees of freedom)
#' @param method name of method for labelling results
#' @return list of error statistics
#' @export
flux_stats <- function(x, n.coeff, method) {
  x <- subset(x, !is.na(Lobs))

  n <- nrow(x)
  dof <- n - n.coeff
  rss_L <- sum(x$Lres^2)
  se_Lres <- sqrt(rss_L/dof)
  se_L <- se_Lres/sqrt(n)
  mean_Lest <- mean(x$Lest)
  sd_Lest <- sd(x$Lest)
  mean_Lobs <- mean(x$Lobs)
  sd_Lobs <- sd(x$Lobs)
  rse_L <- se_L/mean_Lest
  cv_Lres <- se_Lres/mean_Lest

  return(list(n=n,
              dof=dof,
              rss_L=rss_L,
              se_Lres=se_Lres,
              se_L=se_L,
              mean_Lest=mean_Lest,
              sd_Lest=sd_Lest,
              mean_Lobs=mean_Lobs,
              sd_Lobs=sd_Lobs,
              rse_L=rse_L,
              cv_Lres=cv_Lres,
              method=method))
}
