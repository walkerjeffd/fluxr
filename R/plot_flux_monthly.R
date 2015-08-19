
#' Plot monthly summary of flux load estimation model
#'
#' @param loads output of flux estimation function
#' @param site name of site
#' @param variable name of variable
#' @param title plot title (if NULL, uses a default)
#' @export
plot_flux_monthly <- function(loads, site, variable, title=NULL) {
  require(ggplot2)
  require(gridExtra)

  x_mon <- loads[['out']][['mon']]
  x_wyr <- loads[['out']][['wyr']]

  mon_stat <- dplyr::mutate(x_mon,
                            MONTH=lubridate::month(MONTHYEAR),
                            Q=Q/N.DAY,
                            L=L/N.DAY)
  mon_stat <- dplyr::group_by(mon_stat, MONTH)
  mon_stat <- dplyr::summarise(mon_stat,
    N=length(MONTH),
    Q_MEAN=mean(Q),
    Q_SD=sd(Q),
    Q_SE=Q_SD/sqrt(N),
    L_MEAN=mean(L),
    L_SD=sd(L),
    L_SE=L_SD/sqrt(N),
    C_MEAN=L_MEAN/Q_MEAN,
    C_SD=sd(C),
    C_SE=C_SD/sqrt(N))

  wyr_stat <- dplyr::mutate(x_wyr,
                            Q=Q/N.DAY,
                            L=L/N.DAY)
  wyr_stat <- dplyr::summarise(wyr_stat,
    MONTH='Year',
    N=length(MONTH),
    Q_MEAN=mean(Q),
    Q_SD=sd(Q),
    Q_SE=Q_SD/sqrt(N),
    L_MEAN=mean(L),
    L_SD=sd(L),
    L_SE=L_SD/sqrt(N),
    C_MEAN=L_MEAN/Q_MEAN,
    C_SD=sd(C),
    C_SE=C_SD/sqrt(N))

  stat <- rbind(mon_stat, wyr_stat)
  stat$MONTH <- ordered(stat$MONTH, levels=c(10, 11, 12, seq(1, 9), 'Year'))

  p_L <- ggplot(x_mon, aes(MONTHYEAR, L)) +
    geom_area(fill='olivedrab3') +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.POSIXt(loads$predict_period[1],
                                       loads$predict_period[2]+lubridate::days(1),
                                       by='year'),
                     labels=scales::date_format('%m/%y')) +
    labs(x="", y="Load (kg/mon)") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,0,0,1), "cm"))

  p_L_mon <- ggplot(stat, aes(MONTH, L_MEAN)) +
    geom_bar(stat='identity', fill='olivedrab3', width=0.6) +
    geom_errorbar(aes(ymin=L_MEAN-L_SE, ymax=L_MEAN+L_SE), width=0.2) +
    labs(x="", y="Load (kg/day)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0,1,0,0.2), "cm"))

  p_C <- ggplot(x_mon, aes(MONTHYEAR, C)) +
    geom_line(color='orangered') +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.POSIXt(loads$predict_period[1],
                                       loads$predict_period[2]+lubridate::days(1),
                                       by='year'),
                     labels=scales::date_format('%m/%y')) +
    geom_hline(yint=0, alpha=0) +
    labs(x="", y="FWM Conc (ppb)") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,0,0,1), "cm"))
  p_C_mon <- ggplot(stat, aes(MONTH, C_MEAN)) +
    geom_bar(stat='identity', fill='orangered', width=0.6) +
    geom_errorbar(aes(ymin=C_MEAN-C_SE, ymax=C_MEAN+C_SE), width=0.2) +
    labs(x="", y="FWM Conc (ppb)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0,1,0,0.2), "cm"))

  p_Q <- ggplot(x_mon, aes(MONTHYEAR, Q)) +
    geom_area(fill='steelblue') +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.POSIXt(loads$predict_period[1],
                                       loads$predict_period[2]+lubridate::days(1),
                                       by='year'),
                     labels=scales::date_format('%m/%y')) +
    geom_hline(yint=0, alpha=0) +
    labs(x="", y="Flow (hm3/mon)") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,0,0,1), "cm"))
  p_Q_mon <- ggplot(stat, aes(MONTH, Q_MEAN)) +
    geom_bar(stat='identity', fill='steelblue', width=0.6) +
    geom_errorbar(aes(ymin=Q_MEAN-Q_SE, ymax=Q_MEAN+Q_SE), width=0.2) +
    labs(x="", y="Flow (hm3/d)") +
    theme_bw() +
    theme(plot.margin=grid::unit(c(0,1,0,0.2), "cm"))

  if (is.null(title))
    title <- paste0('\n',
                    'Concentrations, Flows, and Loads by Month\n',
                    paste(paste0('Site: ', site),
                          paste0('Variable: ', variable),
                          paste0('Dates: ', paste(format(loads$predict_period, '%m/%d/%Y'), collapse=' - ')),
                          sep='   |   '),
                    '\n')

  gridExtra::grid.arrange(grobs=list(gridExtra::arrangeGrob(p_C, p_Q, p_L, ncol=1),
                                     gridExtra::arrangeGrob(p_C_mon, p_Q_mon, p_L_mon, ncol=1)),
                          ncol=2,
                          widths=c(2/3, 1/3),
                          top=title)
}
