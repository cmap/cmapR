#' Compoute robust z-scores 
#' 
#' @description
#' robust zscore implementation
#' takes in a 1D vector, returns 1D vector
#' after computing robust zscores
#' rZ = (x-med(x))/mad(x)
#' 
#' @param x numeric vector to z-score
#' @param min_mad the minimum allowed MAD,
#'   useful for avoiding division by very
#'   small numbers
#' @param ... further options to median, max functions
#'   
#' @return transformed version of x
#' 
#' @examples 
#' (x <- rnorm(25))
#' (robust.zscore(x))
#' 
#' # with min_mad
#' (robust.zscore(x, min_mad=1e-4))
#' 
#' @export
robust.zscore <- function(x, min_mad=1e-6, ...) {
  med <- median(x, ...)        # median
  abs_dev <- abs(x - med) # absolute deviation
  mad <- median(abs_dev, ...)  # median absolute deviation
  # if the MAD is zero, try to estimate from the 
  # data by using the max instead of median, or the
  # min_mad value supplied as an argument, whichever
  # is larger
  if (mad==0) {
    mad <- max(max(abs_dev, ...), min_mad, ...)
  }
  return((x - med) / (mad *1.4826))
}