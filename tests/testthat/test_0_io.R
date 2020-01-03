context("Testing io methods")

test_that("GCTX parsing works", {
	ds <- parse.gctx("test_n5x10.gctx")
	expect_equal(nrow(ds@mat), 10)
	expect_equal(ncol(ds@mat), 5)
	expect_true(is.data.frame(ds@cdesc))
	expect_true(is.data.frame(ds@rdesc))
	expect_equal(length(ds@rid), 10)
	expect_equal(length(ds@cid), 5)
	})

test_that("GCT parsing works", {
	ds <- parse.gctx("test_n5x10.gct")
	expect_equal(nrow(ds@mat), 10)
	expect_equal(ncol(ds@mat), 5)
	expect_true(is.data.frame(ds@cdesc))
	expect_true(is.data.frame(ds@rdesc))
	expect_equal(length(ds@rid), 10)
	expect_equal(length(ds@cid), 5)
	})
 
test_that("GCT parsing ignores rid, cid flags with warning", {
	expect_warning(ds <- parse.gctx("test_n5x10.gct",
		rid = "foo", cid = "bar"))
	expect_equal(nrow(ds@mat), 10)
	expect_equal(ncol(ds@mat), 5)
	expect_true(is.data.frame(ds@cdesc))
	expect_true(is.data.frame(ds@rdesc))
	expect_equal(length(ds@rid), 10)
	expect_equal(length(ds@cid), 5)
	})

test_that("GCTX parsing correctly handles rid or cid that do not exist in dataset", {
  # handle the case when a subset of requested rid / cid are bogus
  expect_warning(
    ds <- parse.gctx("test_n5x10.gctx",
                      rid = c("foo", "200814_at"),
                      cid = c("foo",
                              "CPC001_HA1E_24H:BRD-A95445494-001-02-9:10")))
  expect_equal(nrow(ds@mat), 1)
  expect_equal(ncol(ds@mat), 1)
  expect_true(is.data.frame(ds@cdesc))
  expect_true(is.data.frame(ds@rdesc))
  expect_equal(length(ds@rid), 1)
  expect_equal(length(ds@cid), 1)
  
  # fail when they're all bogus
  expect_error(ds <- parse.gctx("test_n5x10.gctx",
                                  rid = c("foo", "bar"),
                                  cid = c("foo", "bar")))
  
  # same as above, using numeric indices
  expect_warning(ds <- parse.gctx("test_n5x10.gctx",
                                  rid = c(0, 10),
                                  cid = c(0, 5)))
  expect_equal(nrow(ds@mat), 1)
  expect_equal(ncol(ds@mat), 1)
  expect_true(is.data.frame(ds@cdesc))
  expect_true(is.data.frame(ds@rdesc))
  expect_equal(length(ds@rid), 1)
  expect_equal(length(ds@cid), 1)
})

test_that("Writing GCTX works when row or column descriptors have just one column", {
  ds <- parse.gctx("test_n5x10.gctx")
  # set rdesc and cdesc to single-column data.frames
  ds@rdesc <- data.frame("id"=ds@rdesc[, 1])
  ds@cdesc <- data.frame("id"=ds@cdesc[, 1])
  write.gctx(ds, "foo.gctx", appenddim = FALSE)
  # remove the file
  file.remove("foo.gctx")
})

test_that("Writing GCT works when row or column descriptors have just one column", {
  ds <- parse.gctx("test_n5x10.gctx")
  # set rdesc and cdesc to single-column data.frames
  ds@rdesc <- data.frame("id"=ds@rdesc[, 1])
  ds@cdesc <- data.frame("id"=ds@cdesc[, 1])
  write.gct(ds, "foo.gct", appenddim = FALSE)
  # remove the file
  file.remove("foo.gct")
})

test_that("fix.datatypes correctly handles variety of data types", {
  # read a table of annotations and force all classes to be character initially
  cdesc <- read.delim("test_cdesc.txt", colClasses = "character")
  # run the fixing
  fixed <- fix.datatypes(cdesc)
  # make sure certain columns are of certain types
  # these fields should be characters
  expect_true(is.character(fixed$pert_id))
  expect_true(is.character(fixed$pert_iname))
  expect_true(is.character(fixed$pert_type))
  # these should all be ints
  expect_true(is.integer(fixed$pert_time))
  expect_true(is.integer(fixed$qc_slope))
  # these should be numeric
  expect_true(is.numeric(fixed$qc_f_logp))
  expect_true(is.numeric(fixed$qc_iqr))
  # sci_note is stored on disk in exponential format, which
  # should be converted to numeric. 
  expect_true(is.numeric(fixed$sci_note))
})

test_that("various built-in functions have been correctly adatped to GCT", {
  # ds <- cmapR::ds
  expect_equal(nrow(ds), nrow(ds@mat))
  expect_equal(ncol(ds), ncol(ds@mat))
  expect_equal(dim(ds), dim(ds@mat))
  expect_equal(range(ds), range(ds@mat))
  expect_equal(min(ds), min(ds@mat))
  expect_equal(max(ds), max(ds@mat))
  expect_equal(diag(ds), diag(ds@mat))
})

test_that("update_gctx works correctly", {
  # make a copy of the example dataset
  fpath <- "test_copy_n5x10.gctx"
  if (file.exists(fpath)) file.remove(fpath)
  file.copy("test_n5x10.gctx", fpath)
  # modify rows 3-7, columns 2-4 to contain all zeros
  m <- matrix(0, nrow=5, ncol=3)
  # update using integer indices
  update_gctx(m, ofile=fpath, rid=3:7, cid=2:4)
  tmp <- parse.gctx(fpath)
  tmp_m <- tmp@mat[3:7, 2:4]
  dimnames(tmp_m) <- NULL
  expect_identical(m, tmp_m)
  # update using character ids
  m2 <- matrix(1, nrow=5, ncol=3)
  rid <- read.gctx.ids("test_n5x10.gctx", dim="row")
  cid <- read.gctx.ids("test_n5x10.gctx", dim="col")
  update_gctx(m2, ofile=fpath, rid=rid[3:7], cid=cid[2:4])
  tmp2 <- parse.gctx(fpath)
  tmp_m2 <- tmp2@mat[3:7, 2:4]
  dimnames(tmp_m2) <- NULL
  expect_identical(m2, tmp_m2)
  # try updating indices that don't exist in the dataset
  # should produce an error
  expect_error(update.gctx(m2, ofile=fpath, rid=3:7, cid=20:30))
  # try updating indices that don't correspond to dims of array
  # should produce an error
  expect_error(update.gctx(m2, ofile=fpath, rid=3:7, cid=1:2))
  expect_error(update.gctx(rep(0, 10), ofile=fpath, rid=3:7, cid=1:2))
  if (file.exists(fpath)) file.remove(fpath)
})

test_that("GCT accessor methods work properly", {
  expect_equal(ds@mat, get_gct_matrix(ds))
  expect_equal(ds@rid, get_gct_ids(ds))
  expect_equal(ds@cid, get_gct_ids(ds, dim="col"))
  expect_error(get_gct_ids(ds, dim="foo"))
  expect_equal(ds@rdesc, get_gct_meta(ds))
  expect_equal(ds@cdesc, get_gct_meta(ds, dim="col"))
  expect_error(get_gct_meta(ds, dim="foo"))
})
