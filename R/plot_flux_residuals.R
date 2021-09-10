
#' Plot residual summary of flux load estimation model
#'
#' @param loads output of flux estimation function
#' @param site name of site
#' @param variable name of variable
#' @param title plot title (if NULL, uses a default)
#' @export
plot_flux_residuals <- function(loads, site, variable, title=NULL) {
  require(ggplot2)
  require(gridExtra)

  start <- min(loads$predict$DATE)
  end <- max(loads$predict$DATE)

  p_Cres_ts <- ggplot(loads[['predict']], aes(DATE)) +
    geom_hline(yintercept=0, color='grey70') +
    geom_point(aes(y=Cres), color='orangered', size=1) +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.Date(ymd(paste(lubridate::year(start), 10, 1, sep='-')),
                                     ymd(paste(lubridate::year(end), 10, 1, sep='-')),
                                     by='year'),
                     labels=scales::date_format('%m/%y')) +
    labs(x="Date", y="Log Residual Conc (ppb)") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0.2,1,0.2,0.2), "cm"))
  p_Cres_jday <- ggplot(loads[['predict']], aes(lubridate::yday(DATE), Cres)) +
    geom_hline(yintercept=0, color='grey70') +
    geom_point(color='orangered', size=1) +
    labs(x="Julian Day", y="Log Residual Conc (ppb)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0.2,1,0.2,0.2), "cm"))
  p_Cres_Lest <- ggplot(loads[['predict']], aes(Lpred, Cres)) +
    geom_hline(yintercept=0, color='grey70') +
    geom_vline(xint=0, color='grey70') +
    geom_point(color='orangered', size=1) +
    labs(x="Predicted Load (kg/d)", y="Log Residual Conc (ppb)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0.2,1,0.2,0.2), "cm"))
  p_Cres_Cest <- ggplot(loads[['predict']], aes(Cpred, Cres)) +
    geom_hline(yintercept=0, color='grey70') +
    geom_vline(xint=0, color='grey70') +
    geom_point(color='orangered', size=1) +
    labs(x="Predicted Conc (ppb)", y="Log Residual Conc (ppb)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0.2,1,0.2,0.2), "cm"))
  p_Cest_C <- ggplot(loads[['predict']], aes(Cpred, Cobs)) +
    geom_point(color='orangered', size=1) +
    geom_smooth(method='lm') +
    geom_abline(linetype=2) +
    geom_hline(yintercept=0, alpha=0) +
    geom_vline(xint=0, alpha=0) +
    labs(x="Predicted Conc (ppb)", y="Observed Conc (ppb)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0.2,1,0.2,0.2), "cm"))
  p_Lest_L <- ggplot(loads[['predict']], aes(Lpred, Lobs)) +
    geom_hline(yintercept=0, color='grey70') +
    geom_vline(xint=0, color='grey70') +
    geom_point(color='orangered', size=1) +
    geom_smooth(method='lm') +
    geom_abline(linetype=2) +
    labs(x="Predicted Load (kg/d)", y="Observed Load (kg/d)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0.2,1,0.2,0.2), "cm"))

  x <- dplyr::filter(loads[['predict']], !is.na(Cobs))
  x <- dplyr::select(x, DATE, Q, Cobs, Cpred)
  x <- tidyr::gather(x, VAR, VALUE, Cobs, Cpred)

  p_C_Q <- ggplot(x, aes(Q, y=VALUE, color=VAR)) +
    geom_hline(yintercept=0, color='grey70') +
    geom_point(size=1) +
    scale_x_log10(breaks=fluxr::log_breaks(seq(1, 9), c(0.001, 0.01, 0.1, 1, 10, 100))) +
    scale_color_manual('',
                       values=c('Cobs'='orangered', 'Cpred'='steelblue'),
                       labels=c('Cobs'='Observed', 'Cpred'='Predicted')) +
    labs(x="Flow (hm3/d)", y="Conc (ppb)") +
    theme_bw() +
    theme(legend.background=element_blank(),
          plot.margin=grid::unit(c(0.2,0.2,0.2,0.2), "cm"),
          panel.grid.minor.x=element_blank(),
          axis.text.x=element_text(angle=45, hjust=1, vjust=1, size=8))
  p_C_jday <- ggplot(x, aes(yday(DATE), y=VALUE, color=VAR)) +
    geom_point(size=1) +
    geom_hline(yintercept=0, alpha=0) +
    scale_color_manual('',
                       values=c('Cobs'='orangered', 'Cpred'='steelblue'),
                       labels=c('Cobs'='Observed', 'Cpred'='Predicted')) +
    labs(x="Julian Day", y="Conc (ppb)") +
    theme_bw() +
    theme(legend.background=element_blank(),
          plot.margin=grid::unit(c(0.2,0.2,0.2,0.2), "cm"),
          panel.grid.minor.x=element_blank())

  if (is.null(title))
    title <- paste0('\n',
                    'Regression Diagnostics\n',
                    paste(paste0('Site: ', site),
                          paste0('Variable: ', variable),
                          paste0('Dates: ', paste(format(loads$predict_period, '%m/%d/%Y'), collapse=' - ')),
                          sep='   |   '),
                    '\n')

  gridExtra::grid.arrange(grobs=list(gridExtra::arrangeGrob(p_Cres_ts, p_Cres_jday, p_Cres_Lest, p_Cres_Cest, p_Cest_C, p_Lest_L, ncol=3),
                                     gridExtra::arrangeGrob(p_C_Q, p_C_jday, ncol=2)),
                          heights=c(2/3, 1/3),
                          top=title)
}
