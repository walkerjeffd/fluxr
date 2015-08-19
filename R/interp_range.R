#' Interpolate missing values with fixed ranges around discrete samples
#'
#' @param x a vector of timesteps (numeric or date-like)
#' @param y a vector of discrete values with NAs indicating missing values
#' @param max_interval the maximum number of timesteps for using linear interpolation
#' @param fill value to fill beginning and ending values outside max_interval
#' @return vector of interpolated values corresponding to x
#' @export
#' @examples
#' interp_range(x=seq(1,13), y=c(NA, NA, 1, NA, NA, 2, NA, NA, NA, 3, NA, NA, NA), max_interval=2, fill=0)
interp_range <- function(x, y, max_interval, fill=0) {
  if (length(x) != length(y)) {
    stop('input vectors x and y must have the same length')
  }
  if (all(!is.na(y))) {
    return(y)
  }
  if (max_interval <= 0) {
    stop('max_interval must be greater than or equal to 0')
  }

  df <- data.frame(X=x, Y=y)

  df[['HAS_VALUE']] <- !is.na(df[['Y']])
  df[['ID']] <- cumsum(df[['HAS_VALUE']])
  df[['ROW']] <- seq(1, nrow(df))

  df_grp <- dplyr::group_by(df, ID)
  df_grp <- dplyr::mutate(df_grp,
                          N_SINCE_PREV=dplyr::row_number(ID)-1,
                          N=max(N_SINCE_PREV))
  df <- dplyr::ungroup(df_grp)


  df[['N_UNTIL_NEXT']] <- df[['N']] - df[['N_SINCE_PREV']]
  df[['N_UNTIL_NEXT']] <- ifelse(df[['ID']] == 0,
                                 df[['N_UNTIL_NEXT']] + 1,
                                 dplyr::lag(df[['N_UNTIL_NEXT']]))
  df[['N_UNTIL_NEXT']] <- ifelse(df[['ID']] == 0 & df[['HAS_VALUE']],
                                 0,
                                 df[['N_UNTIL_NEXT']])
  df[['N_UNTIL_NEXT']] <- ifelse(df[['ID']] == max(df[['ID']]) & df[['N_SINCE_PREV']] > 0,
                                 NA_integer_,
                                 df[['N_UNTIL_NEXT']])
  df[['N_SINCE_PREV']] <- ifelse(df[['ID']] == 0,
                                 NA_integer_,
                                 df[['N_SINCE_PREV']])

  df[['N']] <- NULL

  df.id <- dplyr::group_by(df, ID)
  df.id <- dplyr::summarise(df.id, X=first(X), Y=first(Y))

  df.id[['X_PREV']] <- ifelse(df.id[['ID']] == 0, NA, df.id[['X']])
  df.id[['X_NEXT']] <- dplyr::lead(df.id[['X']])
  df.id[['Y_PREV']] <- df.id[['Y']]
  df.id[['Y_NEXT']] <- dplyr::lead(df.id[['Y']])

  df[['X']] <- NULL
  df[['Y']] <- NULL

  df <- dplyr::left_join(df, df.id, by='ID')

  df[['Z']] <- approx(df[['X']], df[['Y']], xout=df[['X']], rule=2)[['y']]

  # outside interval before first
  df[['Z']] <- ifelse(df[['ID']] == 0,
                      ifelse(df[['N_UNTIL_NEXT']] > max_interval,
                             fill,
                             df[['Z']]),
                      df[['Z']])
  # outside interval after last
  df[['Z']] <- ifelse(df[['ID']] == max(df[['ID']]),
                      ifelse(df[['N_SINCE_PREV']] > max_interval,
                             fill,
                             df[['Z']]),
                      df[['Z']])

  # inside interval from prev, outside interval from next
  df[['Z']] <- ifelse(df[['ID']] > 0 &
                        df[['ID']] < max(df[['ID']]) &
                        df[['N_SINCE_PREV']] <= max_interval &
                        df[['N_UNTIL_NEXT']] > max_interval,
                      df[['Y_PREV']],
                      df[['Z']])

  # outside interval from prev, inside interval from next
  df[['Z']] <- ifelse(df[['ID']] > 0 &
                        df[['ID']] < max(df[['ID']]) &
                        df[['N_SINCE_PREV']] > max_interval &
                        df[['N_UNTIL_NEXT']] <= max_interval,
                      df[['Y_NEXT']],
                      df[['Z']])
  return(df[['Z']])
}
