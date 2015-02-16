#' Estimate daily loads using flow-weighted mean concentration by year
#'
#' @param df data frame containing observed concentrations, flows, and loads
#' @param n_min minimum number of samples to use fwm concentration by water year
#' @return input data frame with estimated concentration and loads
#' @export
flux_fwm <- function(df, n_min=5) {
  require(dplyr)
  df.sample <- filter(df, !is.na(L))
  print(paste(df$SITE_NAME[1], df$VAR[1], sep='|'))
  FWM.all <- sum(df.sample$L)/sum(df.sample$Q)

  FWM.wyr <- group_by(df.sample, WYEAR) %>%
    summarise(N.wyr=n(),
              FWM.wyr=sum(L)/sum(Q))

  df <- left_join(df, FWM.wyr) %>%
    mutate(FWM=FWM.all,
           Cest=ifelse(N.wyr>=n_min, FWM.wyr, FWM),
           Cest.which=ifelse(N.wyr>=n_min, "FWM.wyr", "FWM"),
           Lest=Cest*Q,
           Lres=L-Lest)

  return(df)
}


