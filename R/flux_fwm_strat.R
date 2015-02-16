#' Estimate daily loads using flow-weighted mean concentration by water year and stratified flow
#'
#' @param df data frame containing observed concentrations, flows, and loads
#' @param n_min minimum number of samples to use fwm concentration by water year
#' @return data frame \code{df} with estimated daily concentration and loads
#' @export
flux_fwm_strat <- function(df, n_min=5) {
  require(dplyr)
  df <- mutate(df,
               Qstrat=ifelse(Q>mean(Q), 2, 1))

  df.sample <- filter(df, !is.na(L))

  FWM.all <- sum(df.sample$L)/sum(df.sample$Q)

  FWM.wyr <- group_by(df.sample, WYEAR) %>%
    summarise(N.wyr=n(),
              FWM.wyr=sum(L)/sum(Q))

  FWM.strat <- group_by(df.sample, Qstrat) %>%
    summarise(N.strat=n(),
              FWM.strat=sum(L)/sum(Q))

  FWM.wyr.strat <- group_by(df.sample, WYEAR, Qstrat) %>%
    summarise(N.wyr.strat=n(),
              FWM.wyr.strat=sum(L)/sum(Q))

  df <- left_join(df, FWM.strat) %>%
    left_join(FWM.wyr) %>%
    left_join(FWM.wyr.strat) %>%
    mutate(FWM=FWM.all,
           Cest=ifelse(N.wyr.strat>=n_min, FWM.wyr.strat,
                       ifelse(N.strat>=n_min, FWM.wyr, FWM.strat)),
           Cest.which=ifelse(N.wyr.strat>=n_min, "FWM.wyr.strat",
                             ifelse(N.strat>=n_min, "FWM.wyr", "FWM.strat")),
           Lest=Cest*Q,
           Lres=L-Lest)

  return(df)
}
