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
  if (max_interval <= 0) {
    stop('max_interval must be greater than or equal to 0')
  }
  if (all(!is.na(y))) {
    return(y)
  }

  df <- data.frame(X = x, Y = y)
  df <- dplyr::mutate(df, HAS_VALUE = !is.na(Y), ID = cumsum(HAS_VALUE))
  df <- dplyr::group_by(df, ID)
  df <- dplyr::mutate(df,
                      N_SINCE_PREV = dplyr::row_number(ID) - 1,
                      N = max(N_SINCE_PREV))
  df <- dplyr::ungroup(df)

  df <- dplyr::mutate(df,
                      N_UNTIL_NEXT = N - N_SINCE_PREV,
                      N_UNTIL_NEXT = ifelse(ID == 0,
                                            N_UNTIL_NEXT + 1,
                                            dplyr::lag(N_UNTIL_NEXT)),
                      N_UNTIL_NEXT = ifelse(dplyr::row_number(ID) == 1 &
                                              HAS_VALUE,
                                            0,
                                            N_UNTIL_NEXT),
                      N_UNTIL_NEXT = ifelse(ID == max(ID) &
                                              N_SINCE_PREV > 0,
                                            NA_integer_,
                                            N_UNTIL_NEXT),
                      N_SINCE_PREV = ifelse(ID == 0,
                                            NA_integer_,
                                            N_SINCE_PREV))
  df <- dplyr::select(df, -N)

  df.id <- dplyr::group_by(df, ID)
  df.id <- dplyr::summarise(df.id,
                            X = dplyr::first(X),
                            Y = dplyr::first(Y))
  df.id <- dplyr::mutate(df.id,
                  X_PREV = ifelse(ID == 0, NA, X),
                  X_NEXT = dplyr::lead(X),
                  Y_PREV = Y,
                  Y_NEXT = dplyr::lead(Y))
  df.id <- dplyr::select(df.id, -X, -Y)

  df <- dplyr::left_join(df, df.id, by="ID")

  df$Z <- approx(df$X, df$Y, xout = df$X, rule = 2)$y

  # outside interval before first
  df <- dplyr::mutate(df, Z = ifelse(ID==0,
                                     ifelse(N_UNTIL_NEXT > max_interval,
                                            fill,
                                            Z),
                                     Z))
  # outside interval after last
  df <- dplyr::mutate(df, Z = ifelse(ID == max(ID),
                                     ifelse(N_SINCE_PREV > max_interval,
                                            fill,
                                            Z),
                                     Z))

  # inside interval from prev, outside interval from next
  df <- dplyr::mutate(df, Z = ifelse(ID > 0 &
                                       ID < max(ID) &
                                       N_SINCE_PREV <= max_interval &
                                       N_UNTIL_NEXT > max_interval,
                                     Y_PREV,
                                     Z))

  # outside interval from prev, inside interval from next
  df <- dplyr::mutate(df, Z = ifelse(ID > 0 &
                                       ID < max(ID) &
                                       N_SINCE_PREV > max_interval &
                                       N_UNTIL_NEXT <= max_interval,
                                     Y_NEXT,
                                     Z))
  return(df$Z)
}
