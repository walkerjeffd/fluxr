
#' Plot summary of flux load estimation model
#'
#' @param loads output of flux estimation function
#' @param site name of site
#' @param variable name of variable
#' @param title plot title (if NULL, uses a default)
#' @export
plot_flux_summary <- function(loads, site, variable, title=NULL) {
  require(ggplot2)
  require(gridExtra)

  x_day <- loads[['predict']]
  x_wyr <- loads[['out']][['wyr']]

  p.conc <- ggplot(x_day, aes(DATE)) +
    geom_line(aes(y=Cest, color='Final'), size=0.25) +
    geom_point(aes(y=Cobs), color='red', size=1) +
    geom_line(aes(y=Cpred, color='Regression'), size=0.25) +
    geom_hline(yint=0, alpha=0) +
    labs(x='', y='Conc (ppb)') +
    scale_color_manual('', values=c('Final'='orangered', 'Regression'='black')) +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.POSIXt(loads[['predict_period']][[1]],
                                       loads[['predict_period']][[2]],
                                       by='year'),
                     labels=scales::date_format('%m/%y')) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,0,0,1), "cm"),
          legend.margin=grid::unit(0, "cm"),
          legend.position=c(1, 1.05),
          legend.justification=c(1, 1),
          legend.direction=c('horizontal'),
          legend.key.size=grid::unit(0.5, "cm"),
          legend.background=element_blank())

  p.flow <- ggplot(x_day, aes(DATE, Qobs)) +
    geom_area(fill='steelblue', alpha=0.5) +
    geom_point(aes(y=Qobs), data=subset(loads$predict, SAMPLED), size=1, color='black') +
    labs(x='', y='Flow (10^6 m3/day)') +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.POSIXt(loads[['predict_period']][[1]],
                                       loads[['predict_period']][[2]],
                                       by='year'),
                     labels=scales::date_format('%m/%y')) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,0,0,1), "cm"))

  p.load <- ggplot(x_day, aes(DATE, Lest)) +
    geom_area(fill='olivedrab3') +
    geom_point(aes(y=Lobs), data=subset(loads$predict, SAMPLED), size=1, color='olivedrab4') +
    labs(x='', y='Load (kg/day)') +
    scale_x_datetime(expand=c(0, 0),
                     breaks=seq.POSIXt(loads[['predict_period']][[1]],
                                       loads[['predict_period']][[2]],
                                       by='year'),
                     labels=scales::date_format('%m/%y')) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,0,0,1), "cm"))

  p.wyr.conc <- ggplot(x_wyr, aes(factor(WYEAR), C)) +
    geom_bar(stat='identity', fill='orangered', width=0.6) +
    geom_errorbar(aes(ymin=C-C_se, ymax=C+C_se), width=0.2) +
    labs(x='', y='FWM Conc (ppb)') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,1,0,0.2), "cm"))

  p.wyr.flow <- ggplot(x_wyr, aes(factor(WYEAR), Q)) +
    geom_bar(stat='identity', fill='steelblue', width=0.6) +
    labs(x='', y='Flow (10^6 m3/yr)') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,1,0,0.2), "cm"))

  p.wyr.load <- ggplot(x_wyr, aes(factor(WYEAR), L)) +
    geom_bar(stat='identity', fill='olivedrab3', width=0.6) +
    geom_errorbar(aes(ymin=L-L_se, ymax=L+L_se), width=0.2) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='Load (kg/yr)') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          plot.margin=grid::unit(c(0,1,0,0.2), "cm"))


  plots <- gridExtra::arrangeGrob(gridExtra::arrangeGrob(p.conc, p.flow, p.load, ncol=1),
                                  gridExtra::arrangeGrob(p.wyr.conc, p.wyr.flow, p.wyr.load, ncol=1),
                                  widths=c(2/3, 1/3), ncol=2)

  tbl1 <- c('Site: '         = site,
            'Variable: '     = variable,
            'Output Period: '= paste(format(loads$predict_period, '%m/%d/%Y'), collapse=' - '),
            'Sample Period: '= paste(format(loads$sample_period, '%m/%d/%Y'), collapse=' - '),
            'No. Samples: '  = nrow(loads$fit))
  tbl2 <- c('Mean Daily Flow: '= paste0(format(mean(x_day[['Q']]), nsmall=2L, digits=0), ' 10^6 m3/d'),
            'Mean Daily Load: '= paste0(format(mean(x_day[['Lest']]), nsmall=1L, digits=0), ' kg/d'),
            'FWM Conc: '       = paste0(format(mean(x_day[['Lest']])/mean(x_day[['Q']]), nsmall=1L, digits=0), ' ppb'),
            ' '=' ',
            ' '=' ')
  tbl3 <- c('Rel. Std. Error: '     = paste0(format(loads$stats$rse_L*100, nsmall=1L, digits=2), '%'),
            'Regression Adj. R^2: ' = paste0(format(loads$stats$lm.adj.r.squared*100, nsmall=1L, digits=0), '%'),
            'Regression SE: '       = format(loads$stats$lm.sigma, nsmall=2L, digits=0),
            ' '=' ',
            ' '=' ')

  thm <- ttheme_minimal(rowhead=list(fg_params=list(fontface=2L, hjust=1)),
                        core=list(fg_params=list(hjust=0, x=0)))
  tbls <- lapply(list(tbl1, tbl2, tbl3), function(x) {
    gridExtra::tableGrob(data.frame(value=unname(x)),
                         rows=names(x), cols=NULL,
                         theme=thm)
  })
  tbls <- gridExtra::arrangeGrob(tbls[[1]], tbls[[2]], tbls[[3]], ncol=4, widths=c(1/3, 1/4, 1/4, 1/6))

  if (is.null(title))
    title <- paste0('\n',
                    'Concentrations, Flows, and Loads by Day and Water Year\n',
                    paste(paste0('Site: ', site),
                          paste0('Variable: ', variable),
                          paste0('Dates: ', paste(format(loads$predict_period, '%m/%d/%Y'), collapse=' - ')),
                          sep='   |   '),
                    '\n')

  gridExtra::grid.arrange(grobs=list(plots, tbls),
    ncol=1, nrow=2,
    heights=c(4/5, 1/5),
    top=title)
}
