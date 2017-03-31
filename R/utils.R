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
