
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

#' Estimate daily loads using flow-weighted mean concentration by water year and stratified flow
#'
#' @param df data frame containing observed concentrations, flows, and loads
#' @param n_min minimum number of samples to use fwm concentration by water year
#' @return input data frame with estimated concentration and loads
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

#' Estimate daily loads using multi-variate regression
#'
#' @param df data frame containing observed concentrations, flows, and loads
#' @param interp flag to use interpolated residuals
#' @param max_interval maximum interval for using interpolation
#' @return input data frame with estimated concentration and loads
#' @export
flux_regression <- function(df, interp=FALSE, max_interval=90) {
  require(dplyr)

  df <- select(df, DATE, Q, C, L) %>%
    arrange(DATE) %>%
    mutate(SAMPLED=!is.na(C))

  df_input <- df %>%
    mutate(JDAY=ifelse(yday(DATE)<60, 60,
                       ifelse(yday(DATE)>300, 300, yday(DATE))),
           LogC=log(C),
           Qderiv=log(Q/lag(Q)),
           Qderiv=ifelse(is.na(Qderiv), 0, Qderiv),
           Year=year(DATE)+JDAY/365.25,
           Year2=Year*Year,
           Cos2t=cos(2*2*pi*JDAY/365.25),
           Sin2t=sin(2*2*pi*JDAY/365.25),
           Cost=cos(2*pi*JDAY/365.25),
           Sint=sin(2*pi*JDAY/365.25),
           LogQ3=log(Q)^3,
           LogQ2=log(Q)^2,
           LogQ=log(Q)) %>%
    filter(!is.na(L))

  Qderiv_range <- range(df_input$Qderiv)
  Q_range <- range(df_input$Q)

  lm_flux <- lm(LogC~Qderiv+Year+Year2+Cos2t+Sin2t+Cost+Sint+LogQ3+LogQ2+LogQ,
                data=df_input)

  df_predict <- mutate(df,
                       JDAY=ifelse(yday(DATE)<60, 60,
                                   ifelse(yday(DATE)>300, 300, yday(DATE))),
                       LogC=log(C),
                       Qderiv=log(Q/lag(Q)),
                       Qderiv=ifelse(is.na(Qderiv), 0, Qderiv),
                       Qderiv=ifelse(Qderiv<Qderiv_range[1], Qderiv_range[1],
                                     ifelse(Qderiv>Qderiv_range[2], Qderiv_range[2], Qderiv)),
                       Year=year(DATE)+JDAY/365.25,
                       Year2=Year*Year,
                       Cos2t=cos(2*2*pi*JDAY/365.25),
                       Sin2t=sin(2*2*pi*JDAY/365.25),
                       Cost=cos(2*pi*JDAY/365.25),
                       Sint=sin(2*pi*JDAY/365.25),
                       Q=ifelse(Q<Q_range[1], Q_range[1],
                                ifelse(Q>Q_range[2], Q_range[2], Q)),
                       LogQ3=log(Q)^3,
                       LogQ2=log(Q)^2,
                       LogQ=log(Q))

  # compute predictions
  df_predict$Cest_model_bias <- exp(predict(lm_flux, df_predict))

  # correct for re-transformation bias
  df_predict <- mutate(df_predict,
                       Cest_model=exp(log(Cest_model_bias) +
                                      log(sum(C/Cest_model_bias, na.rm=TRUE) /
                                          sum(!is.na(C)))))

#   df <- merge(df, select(df_predict, DATE, Cest))

  if (interp) {
    df_predict <- mutate(df_predict,
                         Cres=log(C/Cest_model),
                         Cres_interp=interp_range(x=DATE, y=Cres,
                                                  max_interval=max_interval, fill=0),
                         Cest=Cest_model*exp(Cres_interp),
                         Lest=Cest*Q,
                         Lest_model=Cest_model*Q,
                         Lres=L-Lest_model)
  } else {
    df_predict <- mutate(df_predict,
                 Cest=Cest_model,
                 Cres=log(C/Cest),
                 Lest=Cest*Q,
                 Lest_model=Cest_model*Q,
                 Lres=L-Lest)
  }


  df_stats <- load_stats(df_predict, n.coeff=9, method="Method 5")

  df_wyr <- mutate(df_predict, WYEAR=wyear(DATE)) %>%
      group_by(WYEAR) %>%
      summarise(N.DAY=n(),
                N.SAMPLE=sum(SAMPLED),
                Q=sum(Q),
                L=sum(Lest, na.rm=TRUE),
                C=L/Q) %>%
      mutate(L_RSE=df_stats$CV_Lresid/sqrt(N.SAMPLE),
             L_SE=L*L_RSE,
             C_SE=C*L_RSE)

  return(list(model=lm_flux,
              obs=df_input,
              pred=df_predict,
              stats=df_stats,
              pred_wyr=df_wyr))
}
