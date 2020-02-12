# this file will contains deprecated functions
# each will warn the user about the deprecation but then
# call the replacement function with the same input

#' @aliases parse_gctx
#' @rdname parse_gctx
#' @keywords internal
parse.gctx <- function(...) {
  warning("parse.gctx has been deprecated. Please use parse_gctx.")
  parse_gctx(...)
}

#' @aliases parse_grp
#' @rdname parse_grp
#' @keywords internal
parse.grp <- function(...) {
  warning("parse.grp has been deprecated. Please use parse_grp.")
  parse_grp(...)
}

#' @aliases parse_gmt
#' @rdname parse_gmt
#' @keywords internal
parse.gctx <- function(...) {
  warning("parse.gmt has been deprecated. Please use parse_gmt.")
  parse_gmt(...)
}

#' @aliases parse_gmx
#' @rdname parse_gmx
#' @keywords internal
parse.gmx <- function(...) {
  warning("parse.gmx has been deprecated. Please use parse_gmx.")
  parse_gmx(...)
}

#' @aliases melt_gct
#' @rdname melt_gct
#' @keywords internal
melt.gct <- function(...) {
  warning("melt.gct has been deprecated. Please use melt_gct.")
  melt_gct(...)
}

#' @aliases annotate_gct
#' @rdname annotate_gct
#' @keywords internal
annotate.gct <- function(...) {
  warning("annotate.gct has been deprecated. Please use annotate_gct")
  annotate_gct(...)
}

#' @aliases subset_gct
#' @rdname subset_gct
#' @keywords internal
subset.gct <- function(...) {
  warning("subset.gct has been deprecated. Please use subset_gct")
  subset_gct(...)
}

#' @aliases update_gct
#' @rdname update_gct
#' @keywords internal
update.gctx <- function(...) {
  warning("update.gctx has been deprecated. Please use update_gctx")
  update_gctx(...)
}

#' @aliases transpose_gct
#' @rdname transpose_gct
#' @keywords internal
transpose.gct <- function(...) {
  warning("transpose.gct has been deprecated. Please use transpose_gct")
  transpose_gct(...)
}

#' @aliases rank_gct
#' @rdname rank_gct
#' @keywords internal
rank.gct <- function(...) {
  warning("rank.gct has been deprecated. Please use rank_gct")
  rank_gct(...)
}

#' @aliases extract_gct
#' @rdname extract_gct
#' @keywords internal
extract.gct <- function(...) {
  warning("extract.gct has been deprecated. Please use extract_gct")
  extract_gct(...)
}

#' @aliases merge_gct
#' @rdname merge_gct
#' @keywords internal
merge.gct <- function(...) {
  warning("merge.gct has been deprecated. Please use merge_gct")
  merge_gct(...)
}

#' @aliases read_gctx_ids
#' @rdname read_gctx_ids
#' @keywords internal
read.gctx.ids <- function(...) {
  warning("read.gctx.ids has been deprecated. Please use read_gctx_ids")
  read_gctx_ids(...)
}

#' @aliases read_gctx_meta
#' @rdname read_gctx_meta
#' @keywords internal
read.gctx.meta <- function(...) {
  warning("read.gctx.meta has been deprecated. Please use read_gctx_meta")
  read_gctx_meta(...)
}

#' @aliases fix_datatypes
#' @rdname fix_datatypes
#' @keywords internal
fix.datatypes <- function(...) {
  warning("fix.datatypes has been deprecated. Please use fix_datatypes")
  fix_datatypes(...)
}

#' @aliases write_gctx
#' @rdname write_gctx
#' @keywords internal
write.gctx <- function(...) {
  warning("write.gctx has been deprecated. Please use write_gctx")
  write_gctx(...)
}

#' @aliases write_gct
#' @rdname write_gct
#' @keywords internal
write.gct <- function(...) {
  warning("write.gct has been deprecated. Please use write_gct")
  write_gct(...)
}

#' @aliases append_dim
#' @rdname append_dim
#' @keywords internal
append.dim <- function(...) {
  warning("append.dim has been deprecated. Please use append_dim")
  append_dim(...)
}

#' @aliases write_gctx_meta
#' @rdname write_gctx_meta
#' @keywords internal
write.gctx.meta <- function(...) {
  warning("write.gctx.meta has been deprecated. Please use write_gctx_meta")
  write_gctx_meta(...)
}

#' @aliases write_tbl
#' @rdname write_tbl
#' @keywords internal
write.tbl <- function(...) {
  warning("write.tbl has been deprecated. Please use write_tbl")
  write_tbl(...)
}
