#' Estimate daily loads using multi-variate regression
#'
#' @param x data frame containing observed concentrations, flows, and loads
#' @param interp flag to interpolate residuals
#' @param max_interval maximum interval for using interpolation
#' @param extrapolate_flow flag to allow extrapolation of flow variables for prediction (Qderiv, Q)
#' @param start_month first month of water year
#' @return list
#' @export
flux_regression <- function(x, interp=FALSE, max_interval=90, extrapolate_flow=FALSE, start_month=10) {
  stopifnot(all(names(x) %in% c('DATE', 'Q', 'C')))

  x_obs <- x

  # sort by DATE
  x <- dplyr::arrange(x, DATE)

  # check for duplicated dates
  stopifnot(sum(duplicated(x[['DATE']])) == 0)

  # check for missing flows
  stopifnot(all(!is.na(x[['Q']])))

  # compute loads
  x <- dplyr::rename(x, Cobs=C)
  x[, 'Lobs'] <- x[, 'Q']*x[, 'Cobs']

  x <- dplyr::mutate(x,
    SAMPLED=!is.na(Cobs),
    WYEAR=wyear(DATE, start_month=start_month),
    MONTH=lubridate::month(DATE),
    JDAY=lubridate::yday(DATE),
    JDAY=ifelse(JDAY<60, 60, JDAY),
    JDAY=ifelse(JDAY>300, 300, JDAY),
    LogCobs=log(Cobs),
    Qderiv=log(Q/lag(Q)),
    Qderiv=ifelse(is.na(Qderiv), 0, Qderiv),
    Year=lubridate::year(DATE)+JDAY/365.25,
    Year2=Year*Year,
    Cos2t=cos(2*2*pi*JDAY/365.25),
    Sin2t=sin(2*2*pi*JDAY/365.25),
    Cost=cos(2*pi*JDAY/365.25),
    Sint=sin(2*pi*JDAY/365.25),
    LogQ3=log(Q)^3,
    LogQ2=log(Q)^2,
    LogQ=log(Q))

  sampled <- which(x[, 'SAMPLED'])

  if (!extrapolate_flow) {
    Qderiv_range <- range(x[sampled, 'Qderiv'])
    Q_range <- range(x[sampled, 'Q'])

    # limit Qderiv and Q to prevent extrapolation
    x <- dplyr::mutate(x,
      Qderiv=ifelse(Qderiv < Qderiv_range[1], Qderiv_range[1], Qderiv),
      Qderiv=ifelse(Qderiv > Qderiv_range[2], Qderiv_range[2], Qderiv),
      Q=ifelse(Q < Q_range[1], Q_range[1], Q),
      Q=ifelse(Q > Q_range[2], Q_range[2], Q),
      LogQ3=log(Q)^3,
      LogQ2=log(Q)^2,
      LogQ=log(Q))
  }

  lm_flux <- lm(LogCobs ~ Qderiv + Year + Year2 + Cos2t + Sin2t + Cost + Sint + LogQ3 + LogQ2 + LogQ,
                data=x[sampled, ])

  # compute predictions
  x_predict <- x
  x_predict[, 'LogCpred_biased'] <- predict(lm_flux, x_predict)

  # correct for re-transformation bias
  sampled <- which(x_predict[, 'SAMPLED'])
  bias_correction <- log(sum(x_predict[sampled, 'Cobs']/exp(x_predict[sampled, 'LogCpred_biased'])) / length(sampled))
  x_predict[, 'LogCpred'] <- x_predict[, 'LogCpred_biased'] + bias_correction
  x_predict[, 'Cpred'] <- exp(x_predict[, 'LogCpred'])

  if (interp) {
    x_predict <- dplyr::mutate(x_predict,
      Cres=log(Cobs/Cpred),
      Cres_interp=interp_range(x=DATE, y=Cres, max_interval=max_interval, fill=0),
      Cest=Cpred*exp(Cres_interp),
      Lpred=Cpred*Q,
      Lest=Cest*Q,
      Lres=Lobs-Lpred)
  } else {
    x_predict <- dplyr::mutate(x_predict,
      Cest=Cpred,
      Cres=log(Cobs/Cest),
      Lest=Cest*Q,
      Lpred=Cpred*Q,
      Lres=Lobs-Lpred)
  }

  # create simplified output data frame
  x_out <- dplyr::select(x_predict, DATE, WYEAR, MONTH, Q, C=Cest, L=Lest)

  x_stats <- flux_stats(x_predict, n.coeff=9, method="Method 5")

  x_wyr <- dplyr::group_by(x_predict, WYEAR)
  x_wyr <- dplyr::summarise(x_wyr,
    N.DAY=n(),
    N.SAMPLE=sum(SAMPLED),
    Q=sum(Q),
    L=sum(Lest, na.rm=TRUE),
    C=L/Q)
  x_wyr <- dplyr::mutate(x_wyr,
    L_rse=x_stats$cv_Lres/sqrt(N.SAMPLE),
    L_se=L*L_rse,
    C_se=C*L_rse)

  sample_period <- range(x_predict[sampled, 'DATE'])
  predict_period <- range(x_predict[, 'DATE'])

  return(list(model=lm_flux,
              obs=x_obs,
              fit=x[sampled, ],
              predict=x_predict,
              out=x_out,
              stats=x_stats,
              wyr=x_wyr,
              sampled=sampled,
              sample_period=sample_period,
              predict_period=predict_period))
}
