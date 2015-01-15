
#' Compute error statistics of load estimates
#'
#' @param df data frame containing observed, predicted and residual loads
#' @param n.coeff number of coefficients used to make predictions (for computing degrees of freedom)
#' @param method name of method for labelling results
#' @return list of error statistics
#' @export
load_stats <- function(df, n.coeff, method) {
  require(dplyr)

  df <- filter(df, !is.na(L))

#   x <- ungroup(df) %>%
#     filter(SAMPLED) %>%
#     summarise(N=n(),
#               DOF=N-n.coeff,
#               RSS_L=sum(Lres^2),
#               SE_Lresid=sqrt(RSS_L/DOF),
#               SE_L=SE_Lresid/sqrt(N)) %>%
#     cbind(MEAN_Lest=filter(df, SAMPLED)$Lest %>% mean(na.rm=TRUE),
#           SD_Lest=filter(df, SAMPLED)$Lest %>% sd(na.rm=TRUE),
#           MEAN_L=filter(df, SAMPLED)$L %>% mean(na.rm=TRUE),
#           SD_L=filter(df, SAMPLED)$L %>% sd(na.rm=TRUE)) %>%
#     mutate(RSE_L=SE_L/MEAN_Lest,
#            CV_Lresid=SE_Lresid/MEAN_Lest,
#            METHOD=method) %>%
#     as.list()
#   return(x)
#
  N <- nrow(df)
  DOF <- N - n.coeff
  RSS_L <- sum(df$Lres^2)
  SE_Lresid <- sqrt(RSS_L/DOF)
  SE_L <- SE_Lresid/sqrt(N)
  MEAN_Lest <- mean(df$Lest)
  SD_Lest <- sd(df$Lest)
  MEAN_L <- mean(df$L)
  SD_L <- sd(df$L)
  RSE_L <- SE_L/MEAN_Lest
  CV_Lresid <- SE_Lresid/MEAN_Lest
  METHOD <- method

  return(list(N=N,
              DOF=DOF,
              RSS_L=RSS_L,
              SE_Lresid=SE_Lresid,
              SE_L=SE_L,
              MEAN_Lest=MEAN_Lest,
              SD_Lest=SD_Lest,
              MEAN_L=MEAN_L,
              SD_L=SD_L,
              RSE_L=RSE_L,
              CV_Lresid=CV_Lresid,
              METHOD=METHOD))
}
