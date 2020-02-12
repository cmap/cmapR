# this file will contains deprecated functions
# each will warn the user about the deprecation but then
# call the replacement function with the same input

#' @aliases parse_gctx
#' @rdname parse_gctx
#' @export
parse.gctx <- function(...) {
  warning("parse.gctx has been deprecated. Please use parse_gctx.")
  parse_gctx(...)
}

#' @aliases parse_grp
#' @rdname parse_grp
#' @export
parse.grp <- function(...) {
  warning("parse.grp has been deprecated. Please use parse_grp.")
  parse_grp(...)
}

#' @aliases parse_gmt
#' @rdname parse_gmt
#' @export
parse.gmt <- function(...) {
  warning("parse.gmt has been deprecated. Please use parse_gmt.")
  parse_gmt(...)
}

#' @aliases parse_gmx
#' @rdname parse_gmx
#' @export
parse.gmx <- function(...) {
  warning("parse.gmx has been deprecated. Please use parse_gmx.")
  parse_gmx(...)
}

#' @aliases melt_gct
#' @rdname melt_gct
#' @export
melt.gct <- function(...) {
  warning("melt.gct has been deprecated. Please use melt_gct.")
  melt_gct(...)
}

#' @aliases annotate_gct
#' @rdname annotate_gct
#' @export
annotate.gct <- function(...) {
  warning("annotate.gct has been deprecated. Please use annotate_gct")
  annotate_gct(...)
}

#' @aliases subset_gct
#' @rdname subset_gct
#' @export
subset.gct <- function(...) {
  warning("subset.gct has been deprecated. Please use subset_gct")
  subset_gct(...)
}

#' @aliases update_gctx
#' @rdname update_gctx
#' @export
update.gctx <- function(...) {
  warning("update.gctx has been deprecated. Please use update_gctx")
  update_gctx(...)
}

#' @aliases transpose_gct
#' @rdname transpose_gct
#' @export
transpose.gct <- function(...) {
  warning("transpose.gct has been deprecated. Please use transpose_gct")
  transpose_gct(...)
}

#' @aliases rank_gct
#' @rdname rank_gct
#' @export
rank.gct <- function(...) {
  warning("rank.gct has been deprecated. Please use rank_gct")
  rank_gct(...)
}

#' @aliases extract_gct
#' @rdname extract_gct
#' @export
extract.gct <- function(...) {
  warning("extract.gct has been deprecated. Please use extract_gct")
  extract_gct(...)
}

#' @aliases merge_gct
#' @rdname merge_gct
#' @export
merge.gct <- function(...) {
  warning("merge.gct has been deprecated. Please use merge_gct")
  merge_gct(...)
}

#' @aliases read_gctx_ids
#' @rdname read_gctx_ids
#' @export
read.gctx.ids <- function(...) {
  warning("read.gctx.ids has been deprecated. Please use read_gctx_ids")
  read_gctx_ids(...)
}

#' @aliases read_gctx_meta
#' @rdname read_gctx_meta
#' @export
read.gctx.meta <- function(...) {
  warning("read.gctx.meta has been deprecated. Please use read_gctx_meta")
  read_gctx_meta(...)
}

#' @aliases fix_datatypes
#' @rdname fix_datatypes
#' @export
fix.datatypes <- function(...) {
  warning("fix.datatypes has been deprecated. Please use fix_datatypes")
  fix_datatypes(...)
}

#' @aliases write_gctx
#' @rdname write_gctx
#' @export
write.gctx <- function(...) {
  warning("write.gctx has been deprecated. Please use write_gctx")
  write_gctx(...)
}

#' @aliases write_gct
#' @rdname write_gct
#' @export
write.gct <- function(...) {
  warning("write.gct has been deprecated. Please use write_gct")
  write_gct(...)
}

#' @aliases append_dim
#' @rdname append_dim
#' @export
append.dim <- function(...) {
  warning("append.dim has been deprecated. Please use append_dim")
  append_dim(...)
}

#' @aliases write_gctx_meta
#' @rdname write_gctx_meta
#' @export
write.gctx.meta <- function(...) {
  warning("write.gctx.meta has been deprecated. Please use write_gctx_meta")
  write_gctx_meta(...)
}

#' @aliases write_tbl
#' @rdname write_tbl
#' @export
write.tbl <- function(...) {
  warning("write.tbl has been deprecated. Please use write_tbl")
  write_tbl(...)
}
