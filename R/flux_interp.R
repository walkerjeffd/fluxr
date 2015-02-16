#' Estimate daily loads using linear interpolation of observed concentration
#'
#' @param df data frame containing observed concentrations, flows, and loads
#' @param max_interval maximum interval for using interpolation
#' @return input data frame with estimated concentration and loads
#' @export
flux_interp <- function(df, max_interval=90) {
  require(dplyr)
  df.sample <- filter(df, !is.na(L))
  FWM <- sum(df.sample$L)/sum(df.sample$Q)
  df$Cest <- interp_range(df$DATE, df$C, max_interval=max_interval, fill=FWM)

  df <- mutate(df, Lest=Cest*Q, Lres=L-Lest)
  return(df)
}

