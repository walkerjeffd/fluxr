#' Get water year component of a date-time.
#'
#' @param x a date-time object
#' @param start_month starting month of the water year
#' @return the water year elements of x as a number
#' @export
#' @examples
#' wyear(lubridate::ymd('2000-09-30') + lubridate::days(c(0,1,2)), start_month=10)
wyear <- function(x, start_month=10) {
  if (start_month==1) {
    return(x)
  } else {
    return(ifelse(lubridate::month(x)>=start_month, lubridate::year(x)+1, lubridate::year(x)))
  }
}
