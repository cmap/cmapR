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
#' (robust_zscore(x))
#' 
#' # with min_mad
#' (robust_zscore(x, min_mad=1e-4))
#' 
#' @export
robust_zscore <- function(x, min_mad=1e-6, ...) {
  med <- stats::median(x, ...)        # median
  abs_dev <- abs(x - med) # absolute deviation
  mad <- stats::median(abs_dev, ...)  # median absolute deviation
  # if the MAD is zero, try to estimate from the 
  # data by using the max instead of median, or the
  # min_mad value supplied as an argument, whichever
  # is larger
  if (mad == 0) {
    mad <- max(max(abs_dev, ...), min_mad, ...)
  }
  return((x - med) / (mad*1.4826))
}

#' Threshold a numeric vector
#' 
#' @param x the vector
#' @param minval minium allowed value
#' @param maxval maximum allowed value
#' 
#' @return a thresholded version of \code{x}
#' 
#' @examples 
#' x <- rnorm(20)
#' threshold(x, -0.1, -0.1)
#' 
#' @export
threshold <- function(x, minval, maxval) {
  # threshold a vector using the minval and maxval supplied
  x[x < minval] <- minval
  x[x > maxval] <- maxval
  return(x)
}

#' Collapse the rows or columns of a matrix using  
#' weighted averaging
#' 
#' @description This is equivalent to the 'modz' procedure
#'   used in collapsing replicates in traditional L1000
#'   data processing. The weight for each replicate is
#'   computed as its normalized average correlation to
#'   the other replicates in the set.
#' 
#' @param m a numeric matrix where the rows or columns are
#'   assumed to be replicates
#' @param dimension the dimension to collapse. either 'row'
#'     or 'col'
#' @param method the correlation method to use
#'
#' @return a list with the following elements
#'  \describe{
#'    \item{values}{a vector of the collapsed values}
#'    \item{correlations}{a vector of the pairwise correlations}
#'    \item{weights}{a vector of the computed weights}
#'  }
#'
#' @examples
#' m <- matrix(rnorm(30), ncol=3)
#' distil(m)
#' 
#' @importFrom matrixStats colAnyNAs
#' 
#' @export
distil <- function(m, dimension="col", method="spearman") {
  if (!is.numeric(m)) {
    stop("m must be numeric")
  }
  if (!(dimension %in% c("row", "col"))) {
    stop("dimension must be either row or col")
  }
  if (dimension == "row") {
    # collapsing across rows, transpose
    m <- t(m)
  }
  # ignore any columns containing NA values
  na_idx <- matrixStats::colAnyNAs(m)
  # make sure to enforce that the resulting object is 
  # a matrix so that cor function will work
  m <- as.matrix(m[, !na_idx])
  # compute pairwise correlation matrix
  # and threshold negative values to 0.01
  corr <- threshold(stats::cor(m, method=method), 0.01, 1)
  # set diagnoal to 0
  diag(corr) <- 0
  row_sums <- rowSums(corr)
  # normalize sums to get weights
  weights <- row_sums / sum(row_sums)
  # multiply input matrix by weights
  weighted_mat <- t(t(m) * weights)
  # and now take the sum
  v <- rowSums(weighted_mat)
  return(list(
    values = v,
    correlations = corr[upper.tri(corr)],
    weights = weights))
}