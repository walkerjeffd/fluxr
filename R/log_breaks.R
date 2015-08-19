
#' Logarithmic breaks
#'
#' @param x vector of first integers
#' @param y vector of magnitudes
#' @export
#' @examples
#' log_breaks(seq(1, 9), c(10, 100, 1000))
log_breaks <- function(x, y) {
  as.vector(outer(x, y, '*'))
}
