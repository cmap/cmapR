context("Testing GCT accessor methods")

test_that("GCT accessor methods work properly", {
  # get the matrix
  expect_equal(ds@mat, mat(ds))
  m <- mat(ds)
  # and reassign it
  tmp <- matrix(0, nrow=nrow(m), ncol=ncol(m))
  mat(ds) <- tmp
  expect_equal(tmp, ds@mat)
  
  # extract row ids
  expect_equal(ds@rid, ids(ds))
  x <- ids(ds)
  # and reassign them
  tmp <- as.character(seq_along(x))
  ids(ds) <- tmp
  expect_equal(tmp, ds@rid)
  
  # extract column ids
  expect_equal(ds@cid, ids(ds, dim="col"))
  x <- ids(ds, dim="col")
  # and reassign them
  tmp <- as.character(seq_along(x))
  ids(ds, dim="col") <- tmp
  expect_equal(tmp, ds@cid)
  
  # extract row meta
  expect_equal(ds@rdesc, meta(ds))
  x <- meta(ds)
  # and reassign it
  tmp <- data.frame(x=sample(letters, nrow(x), replace=TRUE))
  meta(ds) <- tmp
  expect_equal(tmp, ds@rdesc)
  
  # extract column meta
  expect_equal(ds@cdesc, meta(ds, dim="col"))
  x <- meta(ds, dim="col")
  # and reassign it
  tmp <- data.frame(x=sample(letters, nrow(x), replace=TRUE))
  meta(ds, dim="col") <- tmp
  expect_equal(tmp, ds@cdesc)
})