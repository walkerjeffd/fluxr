#' Convert flow units from cfs to hm3/day
#'
#' @param x vector of flows in cfs
#' @return vector of flows in hm3/day
#' @export
#' @examples
#' cfs_to_hm3d(c(1.2, 3.4, 12.3))
cfs_to_hm3d <- function(x) {
  return(x*24*3600/(3.28^3)/1e6)
}
