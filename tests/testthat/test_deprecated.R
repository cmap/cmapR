context("Testing deprecated functions")

test_that("parse.gctx redirects properly", {
  expect_warning({
    test_ds <- parse.gctx("test_n5x10.gctx")
  })
})

test_that("melt.gct redirects properly", {
  expect_warning({
    foo <- melt.gct(ds)
  })
})

test_that("annotate.gct redirects properly", {
  expect_warning({
    newds <- ds
    col_meta <- ds@cdesc
    newds@cdesc <- data.frame(id=ds@cid)
    newds <- annotate.gct(newds, col_meta, dim="column", keyfield="id")
  })
})

test_that("subset.gct redirects properly", {
  expect_warning({
    foo <- subset.gct(ds, cid=1:3)
  })
})

test_that("update.gct redirects properly", {
  expect_warning({
    # make a copy of the example dataset
    fpath <- "test_copy_n5x10.gctx"
    if (file.exists(fpath)) file.remove(fpath)
    file.copy("test_n5x10.gctx", fpath)
    # modify rows 3-7, columns 2-4 to contain all zeros
    m <- matrix(0, nrow=5, ncol=3)
    # update using integer indices
    update.gctx(m, ofile=fpath, rid=3:7, cid=2:4)
    file.remove(fpath)
  })
})

test_that("transpose.gct redirects properly", {
  expect_warning({
    foo <- transpose.gct(ds)
  })
})

test_that("rank.gct redirects properly", {
  expect_warning({
    foo <- rank.gct(ds)
  })
})

test_that("extract.gct redirects properly", {
  expect_warning({
    foo <- extract.gct(kd_gct, row_field="pr_gene_symbol",
                                   col_field="pert_mfc_desc")
  })
})

test_that("merge.gct redirects properly", {
  # duplicate ds and change cid
  ds2 <- ds
  ds2@cid <- ds2@cdesc$id <- colnames(ds2@mat) <- paste("foo", ds2@cid, sep=":")
  expect_warning({
    foo <- merge.gct(ds, ds2, dim="col")
  })
})

test_that("read.gctx.ids redirects properly", {
  expect_warning({
    foo <- read.gctx.ids("test_n5x10.gctx")
  })
})

test_that("read.gctx.meta redirects properly", {
  expect_warning({
    foo <- read.gctx.meta("test_n5x10.gctx")
  })
})

test_that("fix.datatypes redirects properly", {
  expect_warning({
    cdesc <- read.delim("test_cdesc.txt", colClasses = "character")
    # run the fixing
    fixed <- fix.datatypes(cdesc)
  })
})

test_that("write.gctx redirects properly", {
  expect_warning({
    write.gctx(ds, "foo")
    file.remove("foo_n272x978.gctx")
  })
})

test_that("write.gct redirects properly", {
  expect_warning({
    write.gct(ds, "foo")
    file.remove("foo_n272x978.gct")
  })
})

test_that("append.dim redirects properly", {
  expect_warning({
    append.dim("foo", ds@mat)
  })
})

test_that("write.gctx.meta redirects properly", {
  expect_warning({
    # make a copy of the example dataset
    fpath <- "test_copy_n5x10.gctx"
    if (file.exists(fpath)) file.remove(fpath)
    file.copy("test_n5x10.gctx", fpath)
    write.gctx.meta(fpath, data.frame(x=1:10))
    file.remove(fpath)
  })
})

test_that("write.tbl redirects properly", {
  expect_warning({
    write.tbl(cdesc_char, "foo.txt")
    file.remove("foo.txt")
  })
})

