#' Transform a GCT object in to a long form \code{\link{data.table}} (aka 'melt')
#' 
#' @description Utilizes the \code{\link{data.table::melt}} function to transform the
#'   matrix into long form. Optionally can include the row and column
#'   annotations in the transformed \code{\link{data.table}}.
#'   
#' @param g the GCT object
#' @param keep_rdesc boolean indicating whether to keep the row
#'   descriptors in the final result
#' @param keep_cdesc boolean indicating whether to keep the column
#'   descriptors in the final result
#' @param remove_symmetries boolean indicating whether to remove
#'   the lower triangle of the matrix (only applies if \code{g@mat} is symmetric)
#' @param suffixes the character suffixes to be applied if there are
#'   collisions between the names of the row and column descriptors
#'   
#' @return a \code{\link{data.table}} object with the row and column ids and the matrix
#'   values and (optinally) the row and column descriptors
#'   
#' @examples 
#' # simple melt, keeping both row and column meta
#' head(melt.gct(ds))
#' 
#' # update row/colum suffixes to indicate rows are genes, columns experiments
#' head(melt.gct(ds, suffixes = c("_gene", "_experiment")))
#' 
#' # ignore row/column meta
#' head(melt.gct(ds, keep_rdesc = F, keep_cdesc = F))
#' 
#' @family GCT utilities
#' @export
setGeneric("melt.gct", function(g, suffixes=NULL, remove_symmetries=F,
                                keep_rdesc=T, keep_cdesc=T) {
  standardGeneric("melt.gct")
})
setMethod("melt.gct", signature("GCT"),
          function(g, suffixes, remove_symmetries, keep_rdesc, keep_cdesc) {
          # melt a gct object's matrix into a data.frame and merge row and column
          # annotations back in, using the provided suffixes
          # assumes rdesc and cdesc data.frames both have an 'id' field.
          # merges row and/or column annotations into the melted matrix as indicated by
          # keep_rdesc and keep_cdesc, respectively.
          # if remove_symmetries, will check whether matrix is symmetric
          # and return only values corresponding to the upper triangle
          # g@rdesc$id <- rownames(g@rdesc)
          # g@cdesc$id <- rownames(g@cdesc)
          # first, check if matrix is symmetric
          # if it is, use only the upper triangle
          message("melting GCT object...")
          mat <- g@mat
          if (remove_symmetries & isSymmetric(mat)) {
            mat[upper.tri(mat, diag=F)] <- NA
          }
          mat <- data.table(mat)
          mat$rid <- g@rid
          d <- melt(mat, id.vars="rid")
          setattr(d, "names", c("id.x", "id.y", "value"))
          d$id.x <- as.character(d$id.x)
          d$id.y <- as.character(d$id.y)
          # standard data.frame subset here to comply with testthat
          d <- d[!is.na(d$value), , with=F]
          if (keep_rdesc & keep_cdesc) {
            # merge back in both row and column descriptors
            setattr(d, "names", c("id", "id.y", "value"))
            d <- merge(d, data.table(g@rdesc), by="id")
            setnames(d, "id", "id.x")
            setnames(d, "id.y", "id")
            setnames(d, "id", "id.y")
          } else if (keep_rdesc) {
            # keep only row descriptors
            rdesc <- data.table(g@rdesc)
            setnames(rdesc, "id", "id.x")
            d <- merge(d, rdesc, by="id.x")
          } else if (keep_cdesc) {
            # keep only column descriptors
            cdesc <- data.table(g@cdesc)
            setnames(cdesc, "id", "id.y")
            d <- merge(d, cdesc, by="id.y")
          }
          # use suffixes if provided
          if (!is.null(suffixes) & length(suffixes) == 2) {
            newnames <- gsub("\\.x", suffixes[1], names(d))
            newnames <- gsub("\\.y", suffixes[2], newnames)
            setattr(d, "names", newnames)
          }
          message("done")
          return(d)
})


#' Check if x is a whole number
#'
#' @param x number to test
#' @param tol the allowed tolerance
#' @return boolean indicating whether x is tol away from a whole number value
#' @examples
#' is.wholenumber(1)
#' is.wholenumber(0.5)
#' @export
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  return(abs(x - round(x)) < tol)
}

#' Check whether \code{test_names} are columns in the \code{\link{data.frame}} df
#' @param test_names a vector of column names to test
#' @param df the \code{\link{data.frame}} to test against
#' @param throw_error boolean indicating whether to throw an error if
#'   any \code{test_names} are not found in \code{df}
#' @return boolean indicating whether or not all \code{test_names} are
#'   columns of \code{df}
#' @examples 
#' check_colnames(c("pert_id", "pert_iname"), cdesc_char)            # TRUE
#' check_colnames(c("pert_id", "foobar"), cdesc_char, throw_error=F) # FALSE, suppress error
#' @export
check_colnames <- function(test_names, df, throw_error=T) {
  # check whether test_names are valid names in df
  # throw error if specified
  diffs <- setdiff(test_names, names(df))
  if (length(diffs) > 0) {
    if (throw_error) {
      stop(paste("the following column names are not found in", deparse(substitute(df)), ":",
                 paste(diffs, collapse=" "), "\n"))
    } else {
      return(F)
    }
  } else {
    return(T)
  }
}

#' Do a robust \code{\link{data.frame}} subset to a set of ids
#' @param df \code{\link{data.frame}} to subset
#' @param ids the ids to subset to
#' @return a subset version of \code{df}
#' @keywords internal
subset_to_ids <- function(df, ids) {
  # helper function to do a robust df subset
  check_colnames("id", df)
  newdf <- data.frame(df[match(ids, df$id), ])
  names(newdf) <- names(df)
  return(newdf)
}


#' Subset a gct object using the provided row and column ids
#'
#' @param g a gct object
#' @param rid a vector of character ids or integer indices for ROWS
#' @param cid a vector of character ids or integer indices for COLUMNS
#' @examples
#' # first 10 rows and columns by index
#' (a <- subset.gct(ds, rid=1:10, cid=1:10))
#' 
#' # first 10 rows and columns using character ids
#' (b <- subset.gct(ds, rid=ds@rid[1:10], cid=ds@cid[1:10]))
#' 
#' identical(a, b) # TRUE
#' 
#' @family GCT utilities
#' @export
setGeneric("subset.gct", function(g, rid=NULL, cid=NULL) {
  standardGeneric("subset.gct")
})
setMethod("subset.gct", signature("GCT"),
          function(g, rid, cid) {
    # ids can either be a vector of character strings corresponding
    # to row / column ids in the gct object, or integer vectors
    # corresponding to row / column indices
    if (is.null(rid)) rid <- g@rid
    if (is.null(cid)) cid <- g@cid
    # see whether we were given characters or integers
    # and handle accordingly
    process_ids <- function(ids, ref_ids, param) {
      # simple helper function to handle id/idx conversion
      # for character or integer ids
      if (is.character(ids)) {
        idx <- which(ref_ids %in% ids)
      } else if (all(is.wholenumber(ids))) {
        idx <- ids
        ids <- ref_ids[idx]
      } else {
        stop(paste(param, "must be character or ingeter"))
      }
      return(list(ids=ids, idx=idx))
    }
    processed_rid <- process_ids(rid, g@rid, "rid")
    processed_cid <- process_ids(cid, g@cid, "cid")
    rid <- processed_rid$ids
    ridx <- processed_rid$idx
    cid <- processed_cid$ids
    cidx <- processed_cid$idx
    sdrow <- setdiff(rid, g@rid)
    sdcol <- setdiff(cid, g@cid)
    if (length(sdrow) > 0) warning("the following rids were not found:\n", paste(sdrow, collapse="\n"))
    if (length(sdcol) > 0) warning("the following cids were not found:\n", paste(sdcol, collapse="\n"))
    newg <- g
    # make sure ordering is right
    rid <- g@rid[ridx]
    cid <- g@cid[cidx]
    newg@mat <- matrix(g@mat[ridx, cidx], nrow=length(rid), ncol=length(cid))
    colnames(newg@mat) <- cid
    rownames(newg@mat) <- rid
    # cdesc <- data.frame(g@cdesc)
    # rdesc <- data.frame(g@rdesc)
    # make sure annotations row ordering matches
    # matrix, rid, and cid
    newg@cdesc <- subset_to_ids(g@cdesc, cid)
    newg@rdesc <- subset_to_ids(g@rdesc, rid)
    newg@rid <- rid
    newg@cid <- cid
    if (any(dim(newg@mat) == 0)) {
      warning("one or more returned dimension is length 0
              check that at least some of the provided rid and/or
              cid values have matches in the GCT object supplied")
    }
    return(newg)
})
