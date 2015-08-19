#' Estimate daily loads using multi-variate regression
#'
#' @param x data frame containing observed concentrations, flows, and loads
#' @param interp flag to interpolate residuals
#' @param max_interval maximum interval for using interpolation
#' @param extrapolate_flow flag to allow extrapolation of flow variables for prediction (Qderiv, Q)
#' @param start_month first month of water year
#' @param predict_wyear_range vector of length 2 specifying range of water years for final predications (or NULL to use span of data)
#' @return list
#' @export
flux_regression <- function(x, interp=FALSE, max_interval=90, extrapolate_flow=FALSE, start_month=10, predict_wyear_range=NULL) {
  stopifnot(all(names(x) %in% c('DATE', 'Q', 'C')))

  x_obs <- x

  # sort by DATE
  x <- x[order(x[['DATE']]), ]

  # check for dates problems
  stopifnot(sum(duplicated(x[['DATE']])) == 0)
  stopifnot(sum(is.na(x[['DATE']])) == 0)

  # check for missing flows
  stopifnot(all(!is.na(x[['Q']])))

  # compute loads
  x[['Cobs']] <- x[['C']]
  x[['C']] <- NULL
  x[['Lobs']] <- x[['Q']]*x[['Cobs']]

  x[['SAMPLED']] <- !is.na(x[['Cobs']])

  x[['WYEAR']] <- wyear(x[['DATE']], start_month=start_month)
  x[['MONTH']] <- lubridate::month(x[['DATE']])
  x[['JDAY']] <- lubridate::yday(x[['DATE']])
  x[['JDAY']] <- ifelse(x[['JDAY']] < 60, 60, x[['JDAY']])
  x[['JDAY']] <- ifelse(x[['JDAY']] > 300, 300, x[['JDAY']])

  x[['LogCobs']] = log(x[['Cobs']])
  x[['Qderiv']] = log(x[['Q']]/dplyr::lag(x[['Q']]))
  x[['Qderiv']] = ifelse(is.na(x[['Qderiv']]), 0, x[['Qderiv']])
  x[['Year']] = lubridate::year(x[['DATE']]) + x[['JDAY']]/365.25
  x[['Year2']] = x[['Year']]*x[['Year']]
  x[['Cos2t']] = cos(2*2*pi*x[['JDAY']]/365.25)
  x[['Sin2t']] = sin(2*2*pi*x[['JDAY']]/365.25)
  x[['Cost']] = cos(2*pi*x[['JDAY']]/365.25)
  x[['Sint']] = sin(2*pi*x[['JDAY']]/365.25)
  x[['LogQ3']] = log(x[['Q']])^3
  x[['LogQ2']] = log(x[['Q']])^2
  x[['LogQ']] = log(x[['Q']])

  sampled <- which(x[['SAMPLED']])

  if (!extrapolate_flow) {
    Qderiv_range <- range(x[sampled, 'Qderiv'])
    Q_range <- range(x[sampled, 'Q'])

    # limit Qderiv and Q to prevent extrapolation
    x[['Qderiv']]=ifelse(x[['Qderiv']] < Qderiv_range[1], Qderiv_range[1], x[['Qderiv']])
    x[['Qderiv']]=ifelse(x[['Qderiv']] > Qderiv_range[2], Qderiv_range[2], x[['Qderiv']])
    x[['Q']]=ifelse(x[['Q']] < Q_range[1], Q_range[1], x[['Q']])
    x[['Q']]=ifelse(x[['Q']] > Q_range[2], Q_range[2], x[['Q']])
    x[['LogQ3']]=log(x[['Q']])^3
    x[['LogQ2']]=log(x[['Q']])^2
    x[['LogQ']]=log(x[['Q']])
  }

  x_fit <- x[sampled, ]

  lm_flux <- lm(LogCobs ~ Qderiv + Year + Year2 + Cos2t + Sin2t + Cost + Sint + LogQ3 + LogQ2 + LogQ,
                data=x_fit)

  # compute predictions
  x_predict <- x

  # correct for re-transformation bias
  x_predict[['LogCpred_biased']] <- predict(lm_flux, x_predict)
  bias_correction <- log(sum(x_predict[sampled, 'Cobs']/exp(x_predict[sampled, 'LogCpred_biased'])) / length(sampled))
  x_predict[['LogCpred']] <- x_predict[['LogCpred_biased']] + bias_correction
  x_predict[['Cpred']] <- exp(x_predict[['LogCpred']])

  if (interp) {
    x_predict[['Cres']] <- log(x_predict[['Cobs']] / x_predict[['Cpred']])
    x_predict[['Cres_interp']] <- interp_range(x = x_predict[['DATE']],
                                               y = x_predict[['Cres']],
                                               max_interval = max_interval,
                                               fill = 0)
    x_predict[['Cest']] <- x_predict[['Cpred']] * exp(x_predict[['Cres_interp']])
  } else {
    x_predict[['Cest']] <- x_predict[['Cpred']]
    x_predict[['Cres']] <- log(x_predict[['Cobs']] / x_predict[['Cest']])
  }

  x_predict[['Lest']] <- x_predict[['Cest']] * x_predict[['Q']]
  x_predict[['Lpred']] <- x_predict[['Cpred']] * x_predict[['Q']]
  x_predict[['Lres']] <- x_predict[['Lobs']] - x_predict[['Lpred']]

  x_stats <- flux_stats(x_predict, lm_flux, n.coeff=9, method="Method 5")

  # create simplified output data frame
  x_day <- data.frame(DATE=x_predict[['DATE']],
                      WYEAR=x_predict[['WYEAR']],
                      MONTH=x_predict[['MONTH']],
                      SAMPLED=x_predict[['SAMPLED']],
                      Q=x_predict[['Q']],
                      C=x_predict[['Cest']],
                      L=x_predict[['Lest']])

  x_wyr <- dplyr::group_by(x_predict, WYEAR)
  x_wyr <- dplyr::summarise(x_wyr,
    N.DAY=length(WYEAR),
    N.SAMPLE=sum(SAMPLED),
    Q=sum(Q),
    L=sum(Lest),
    C=L/Q)

  x_wyr[['L_rse']] <- x_stats[['cv_Lres']] / sqrt(x_wyr[['N.SAMPLE']])
  x_wyr[['L_se']] <- x_wyr[['L']] * x_wyr[['L_rse']]
  x_wyr[['C_se']] <- x_wyr[['C']] * x_wyr[['L_rse']]

  x_mon <- dplyr::mutate(x_predict, MONTHYEAR=lubridate::floor_date(DATE, unit="month"))
  x_mon <- dplyr::group_by(x_mon, MONTHYEAR, MONTH, WYEAR)
  x_mon <- dplyr::summarise(x_mon,
    N.DAY=length(WYEAR),
    N.SAMPLE=sum(SAMPLED),
    Q=sum(Q),
    L=sum(Lest, na.rm=TRUE),
    C=L/Q)

  if (!is.null(predict_wyear_range)) {
    x_day <- dplyr::filter(x_day, WYEAR %in% seq(predict_wyear_range[[1]], predict_wyear_range[[2]], by=1))
    x_mon <- dplyr::filter(x_mon, WYEAR %in% seq(predict_wyear_range[[1]], predict_wyear_range[[2]], by=1))
    x_wyr <- dplyr::filter(x_wyr, WYEAR %in% seq(predict_wyear_range[[1]], predict_wyear_range[[2]], by=1))
  }

  sample_period <- range(x_fit[['DATE']])
  predict_period <- range(x_day[['DATE']])

  return(list(model=lm_flux,
              obs=as.data.frame(x_obs),
              fit=as.data.frame(x_fit),
              out=list(day=as.data.frame(x_day),
                       mon=as.data.frame(x_mon),
                       wyr=as.data.frame(x_wyr)),
              stats=x_stats,
              predict=as.data.frame(x_predict),
              sampled=sampled,
              sample_period=sample_period,
              predict_period=predict_period
    )
  )
}
