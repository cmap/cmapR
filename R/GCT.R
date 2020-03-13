########################################
### GCT class and method definitions ###
########################################

#' An S4 class to represent a GCT object
#' 
#' @slot mat a numeric matrix
#' @slot rid a character vector of row ids
#' @slot cid a character vector of column ids
#' @slot rdesc a \code{data.frame} of row descriptors
#' @slot rdesc a \code{data.frame} of column descriptors
#' @slot src a character indicating the source (usually file path) of the data
#' 
#' @description The GCT class serves to represent annotated
#'   matrices. The \code{mat} slot contains said data and the
#'   \code{rdesc} and \code{cdesc} slots contain data frames with
#'   annotations about the rows and columns, respectively
#'   
#' @seealso \code{\link{parse_gctx}},
#' \code{\link{write_gctx}}, \code{\link{read_gctx_meta}},
#' \code{\link{read_gctx_ids}}
#' @seealso visit \url{http://clue.io/help} for more information on the
#'   GCT format
methods::setClass("GCT",
                  methods::representation(
                    mat = "matrix",
                    rid = "character",
                    cid = "character",
                    rdesc = "data.frame",
                    cdesc = "data.frame",
                    version = "character",
                    src = "character"
                  )
)


## ----set up methods for checking GCT validity----
methods::setValidity("GCT",
                     function(object) {
                       # check whether dimensions of various
                       # slots are in sync
                       m <- mat(object)
                       rid <- ids(object)
                       cid <- ids(object, dim="column")
                       rdesc <- meta(object)
                       cdesc <- meta(object, dim="column")
                       nrows <- nrow(m)
                       ncols <- ncol(m)
                       if (nrows != length(rid)) {
                         return(
                      "rid must be the same length as number of matrix rows")
                       }
                       if (ncols != length(cid)) {
                         return(
                    "cid must be the same length as number of matrix columns")
                       }
                       if (any(duplicated(cid))) {
                         return("cid must be unique")
                       }
                       if (any(duplicated(rid))) {
                         return("rid must be unique")
                       }
                       if (nrow(cdesc) != ncols & nrow(cdesc) != 0) {
                         return(paste(
                           "cdesc must either have 0 rows or the",
                           "same number of rows as matrix has columns"))
                       }
                       if (nrow(rdesc) != nrows & nrow(rdesc) != 0) {
                         return(paste(
                           "rdesc must either have 0 rows or the same number",
                           "of rows as matrix has rows"))
                       }
                       else {
                         return(T)
                       }
                     }
)

## ----define the initialization method for the GCT class----
methods::setMethod("initialize",
                   signature = "GCT",
                   definition = function(.Object, mat=NULL, rdesc=NULL,
                                         cdesc=NULL,
                                         src=NULL, rid=NULL, cid=NULL,
                                         matrix_only=FALSE) {
                     # if we were supplied a matrix and annotations, use them
                     if (!is.null(mat)) {
                       .Object@mat <- mat
                       # if given rid and cid, use those as well
                       if (!is.null(rid)) {
                         .Object@rid <- rid
                       } else {
                         .Object@rid <- rownames(mat)
                       }
                       if (!is.null(cid)) {
                         .Object@cid <- cid
                       } else {
                         .Object@cid <- colnames(mat)
                       }
                       if (!is.null(rdesc)) {
                         .Object@rdesc <- rdesc
                       }
                       if (!is.null(cdesc)) {
                         .Object@cdesc <- cdesc
                       }
                     } else if (!is.null(src)) {
                       # we were not given a matrix, were we given a src file?
                       # check to make sure it's either .gct or .gctx
                       if (! (grepl(".gct$", src) || grepl(".gctx$", src) ))
                         stop("Either a .gct or .gctx file must be given")
                       if (grepl(".gct$", src)) {
                         if ( ! is.null(rid) || !is.null(cid) )
                           warning(
                             "rid and cid values may only be given for",
                             ".gctx files, ignoring")
                         # parse the .gct
                         .Object@src <- src
                         # get the .gct version by reading first line
                         .Object@version <- scan(src, what="", nlines=1,
                                                 sep="\t", quiet=TRUE)[1]
                         # get matrix dimensions by reading second line
                         dimensions <- scan(src, what=double(0), nlines=1,
                                            skip=1, sep="\t", quiet=TRUE)
                         nrmat <- dimensions[1]
                         ncmat <- dimensions[2]
                         if (length(dimensions) == 4) {
                           # a #1.3 file
                           message("parsing as GCT v1.3")
                           nrhd <- dimensions[3]
                           nchd <- dimensions[4]
                         } else {
                           # a #1.2 file
                           message("parsing as GCT v1.2")
                           nrhd <- 0
                           nchd <- 0
                         }
                         message(paste(
                           src, nrmat, "rows,", ncmat, "cols,", nrhd,
                           "row descriptors,", nchd, "col descriptors"))
                         # read in header line
                         header <- scan(src, what="", nlines=1, skip=2,
                                        sep="\t", quote=NULL, quiet=TRUE)
                         # construct row header and column id's from the
                         # header line
                         if ( nrhd > 0 ) {
                           rhd <- header[2:(nrhd+1)]
                           cid <- header[-(nrhd+1):-1]
                           col_offset <- 1
                         }
                         else {
                           if (any(grepl("description", header,
                                         ignore.case=T))) {
                             # check for presence of description column
                             # in v1.2 files
                             col_offset <- 2
                           } else {
                             col_offset <- 1
                           }
                           rhd <- NULL
                           cid <- header[(1+col_offset):length(header)]
                         }
                         # read in the next set of headers (column annotations)
                         # and shape into a matrix
                         if ( nchd > 0 ) {
                           header <- scan(src, what="", nlines=nchd,
                                          skip=3,
                                          sep="\t", quote=NULL, quiet=TRUE)		
                           header <- matrix(header, nrow=nchd, 
                                            ncol=ncmat + nrhd + 1, byrow=TRUE)
                           # extract the column header and column descriptions
                           chd <- header[,1]
                           cdesc <- header[,-(nrhd+1):-1]
                           # need to transpose in the case where there's
                           # only one column annotation
                           if ( nchd == 1 )
                             cdesc <- t(cdesc)
                         }
                         else {
                           chd = NULL
                           cdesc <- data.frame(id=cid)
                         }
                         # read in the data matrix and row descriptions
                         # shape into a matrix
                         m <- scan(src, what="", nlines=nrmat, 
                                     skip=3 + nchd, sep="\t", quote=NULL,
                                     quiet=TRUE)
                         m <- matrix(m, nrow=nrmat,
                                       ncol=ncmat + nrhd + col_offset, 
                                       byrow=TRUE)
                         # message(paste(dim(mat), collapse="\t"))
                         # Extract the row id's row descriptions,
                         # and the data matrix
                         rid <- m[,1]
                         if ( nrhd > 0 ) {
                           # need as.matrix for the case where there's
                           # only one row annotation
                           rdesc <- as.matrix(m[,2:(nrhd + 1)])
                           m <- matrix(as.numeric(m[,-(nrhd + 1):-1]),
                                         nrow=nrmat, ncol=ncmat)
                         }
                         else {
                           rdesc <- data.frame(id=rid)
                           m <- matrix(as.numeric(
                             m[, (1+col_offset):ncol(m)]),
                             nrow=nrmat, ncol=ncmat)
                         }
                         # assign names to the data matrix and the
                         # row and column descriptions
                         dimnames(m) <- list(rid, cid)
                         if ( nrhd > 0 ) {
                           dimnames(rdesc) <- list(rid, rhd)
                           rdesc <- as.data.frame(rdesc,
                                                  stringsAsFactors=FALSE)
                         }
                         if ( nchd > 0 ) {
                           cdesc <- t(cdesc)
                           dimnames(cdesc) <- list(cid, chd)
                           cdesc <- as.data.frame(cdesc,
                                                  stringsAsFactors=FALSE)
                         }
                         # assign to the GCT slots
                         .Object@mat <- m
                         .Object@rid <- rownames(m)
                         .Object@cid <- colnames(m)
                         if (!matrix_only) {
                           # return annotations as well as matrix
                           .Object@rdesc <- fix_datatypes(rdesc)
                           .Object@cdesc <- fix_datatypes(cdesc)
                           # add id columns to rdesc and cdesc
                           .Object@rdesc$id <- rownames(.Object@rdesc)
                           .Object@cdesc$id <- rownames(.Object@cdesc)
                         }
                       }
                       else { 
                         # parse the .gctx
                         message("reading ", src)
                         .Object@src <- src
                         # if the rid's or column id's are .grp files,
                         # read them in
                         if ( length(rid) == 1 && grepl(".grp$", rid) )
                           rid <- parse_grp(rid)
                         if ( length(cid) == 1 && grepl(".grp$", cid) )
                           cid <- parse_grp(cid)
                         # get all the row and column ids
                         all_rid <- read_gctx_ids(src, dim="row")
                         all_cid <- read_gctx_ids(src, dim="col")
                         # if rid or cid specified, read only those rows/columns
                         # if already numeric, use as is
                         # else convert to numeric indices
                         processed_rids <- process_ids(rid, all_rid, type="rid")
                         processed_cids <- process_ids(cid, all_cid, type="cid")
                         # read the data matrix
                         .Object@mat <-
                           rhdf5::h5read(
                             src, name="0/DATA/0/matrix",
                             index=list(processed_rids$idx, processed_cids$idx))
                         # set the row and column ids, casting as characters
                         .Object@rid <- processed_rids$ids
                         .Object@cid <- processed_cids$ids
                         rownames(.Object@mat) <- processed_rids$ids
                         colnames(.Object@mat) <- processed_cids$ids
                         # get the meta data
                         if (!matrix_only) {
                           .Object@rdesc <- read_gctx_meta(
                             src, dim="row",
                             ids=processed_rids$ids)
                           .Object@cdesc <- read_gctx_meta(
                             src, dim="col",
                             ids=processed_cids$ids)
                         }
                         else {
                           .Object@rdesc <- data.frame(id=.Object@rid,
                                                       stringsAsFactors=FALSE)
                           .Object@cdesc <- data.frame(id=.Object@cid,
                                                       stringsAsFactors=FALSE)
                         }
                         # close any open handles and return the object
                         if(utils::packageVersion('rhdf5') < "2.23.0") {
                           rhdf5::H5close()
                         } else {
                           rhdf5::h5closeAll()
                         }
                         message("done")
                       }
                     }
                     # finally, make sure object is valid before returning
                     ok <- methods::validObject(.Object)
                     return(.Object)
                   }
)

#' Initialize an object of class \code{GCT}
#' @param mat a matrix
#' @param rdesc a \code{data.frame} of row metadata
#' @param cdesc a \code{data.frame} of column metadata
#' @param src path to a GCT file to read
#' @param rid vector of character identifiers for rows
#' @param cid vector of character identifiers for columns
#' @param matrix_only logical indicating whether to read just the matrix
#'   data from \code{src}
#'   
#' @details 
#'   If \code{mat} is provided, \code{rid} and \code{cid} are treated as
#'   the row and column identifiers for the matrix and are assigned to the
#'   \code{rid} and \code{cid} slots of the \code{GCT} object.
#'   
#'   If \code{mat} is not provided but \code{src} is provided,
#'   \code{rid} and \code{cid} are treated as filters. Data will be read from
#'   the file path provided to \code{src} and will then be restricted to the
#'   character ids or integer indices provided to \code{rid} and \code{cid}.
#'   In a similar manner, \code{matrix_only} controls whether the
#'   row and column metadata are also read from the \code{src} file path.
#'   
#' @returns a \code{GCT} object
#' @examples 
#' # an empty object
#' (g <- GCT())
#' # with a matrix
#' (g <- GCT(mat=matrix(rnorm(100), nrow=10)))
#' # from file
#' gct_file <- system.file("extdata", "modzs_n25x50.gctx", package="cmapR")
#' (g <- GCT(src=gct_file))
#' @family GCTX parsing functions
#' @export
GCT <- function(mat=NULL, rdesc=NULL, cdesc=NULL,
                src=NULL, rid=NULL, cid=NULL,
                matrix_only=FALSE) {
  methods::new("GCT", mat=mat, rdesc=rdesc, cdesc=cdesc,
      src=src, rid=rid, cid=cid, matrix_only=matrix_only)
}

###########################################
### accessor functions for GCT objects  ###
###########################################

# set method for displaying a GCT object
# just use the 'str' function to show its structure
setMethod("show", methods::signature("GCT"), function(object) {
  utils::str(object)
})

#' Extract or set the matrix of GCT object
#' @param g the GCT object
#' @return a matrix
#' @examples 
#' # get the matrix
#' m <- mat(ds)
#' # set the matrix
#' mat(ds) <- matrix(0, nrow=nrow(m), ncol=ncol(m))
#' @family GCT accessor methods
#' @export
methods::setGeneric("mat", function(g) {
  standardGeneric("mat")
})
#' @rdname mat
methods::setMethod("mat", "GCT", function(g) g@mat)
methods::setGeneric("mat<-", function(g, value) {
  standardGeneric("mat<-")
})
#' @rdname mat
methods::setMethod("mat<-", "GCT", function(g, value) {
  g@mat <- value
  methods::validObject(g)
  return(g)
})

#' Extract the or set row or column ids of a GCT object
#' @param g the GCT object
#' @return a vector of row ids
#' @examples 
#' # extract rids
#' rids <- ids(ds)
#' # extract column ids
#' cids <- ids(ds, "column")
#' # set rids
#' ids(ds) <- as.character(1:length(rid))
#' # set cids
#' ids(ds, "column") <- as.character(1:length(cid))
#' @family GCT accessor methods
#' @export
methods::setGeneric("ids", function(g, dimension="row")  {
  standardGeneric("ids")
})
#' @rdname ids
methods::setMethod("ids", "GCT", function(g, dimension="row") {
  dimension <- tolower(dimension)
  if (dimension == "col") dimension <- "column"
  stopifnot(dimension %in% c("row", "column"))
  switch(dimension, row=g@rid, column=g@cid)
})
methods::setGeneric("ids<-", function(g, dimension="row", value)  {
  standardGeneric("ids<-")
})
#' @rdname ids
methods::setMethod("ids<-", "GCT", function(g, dimension="row", value) {
  dimension <- tolower(dimension)
  if (dimension == "col") dimension <- "column"
  stopifnot(dimension %in% c("row", "column"))
  if (dimension == "row") {
    g@rid <- value
  } else {
    g@cid <- value
  }
  methods::validObject(g)
  return(g)
})

#' Extract the or set metadata of a GCT object
#' @param g the GCT object
#' @return a data.frame
#' @examples 
#' # extract rdesc
#' rdesc <- meta(ds)
#' # extract cdesc
#' cdec <- meta(ds, dim="column")
#' # set rdesc
#' meta(ds) <- data.frame(x=sample(letters, nrow(rdesc), replace=TRUE))
#' # set cdesc
#' meta(ds, dim="column") <- data.frame(x=sample(letters, nrow(cdesc),
#'   replace=TRUE))
#' @family GCT accessor methods
#' @export
methods::setGeneric("meta", function(g, dimension="row")  {
  standardGeneric("meta")
})
#' @rdname meta
methods::setMethod("meta", "GCT", function(g, dimension="row") {
  dimension <- tolower(dimension)
  if (dimension == "col") dimension <- "column"
  stopifnot(dimension %in% c("row", "column"))
  switch(dimension, row=g@rdesc, column=g@cdesc)
})
methods::setGeneric("meta<-", function(g, dimension="row", value)  {
  standardGeneric("meta<-")
})
#' @rdname meta
methods::setMethod("meta<-", "GCT", function(g, dimension="row", value) {
  dimension <- tolower(dimension)
  if (dimension == "col") dimension <- "column"
  stopifnot(dimension %in% c("row", "column"))
  if (dimension == "row") {
    g@rdesc <- value
  } else {
    g@cdesc <- value
  }
  methods::validObject(g)
  return(g)
})


###########################################
###  cast GCT as SummarizedExperiment   ###
###########################################

#' as("GCT", "SummarizedExperiment")
#' 
#' Create SummarizedExperiment object from GCT object.
#' 
#' @examples
#'
#' se <- as(ds, "SummarizedExperiment")
#' 
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom methods validObject
setAs("GCT", "SummarizedExperiment", function(from) {
  stopifnot(methods::validObject(from))
  SummarizedExperiment::SummarizedExperiment(
    assays = list(exprs = mat(from)), 
    colData = meta(from, dim="column"),
    rowData = meta(from))
})
