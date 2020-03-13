context("Testing GCT class and accessor methods")

test_that("GCT constructor works properly", {
  # initialize empty object
  g <- GCT()
  expect_true(is(g, "GCT"))
  expect_equal(dim(g@mat), c(0, 0))
  expect_equal(nrow(g@rdesc), 0)
  expect_equal(nrow(g@cdesc), 0)
  # try with matrix
  g <- GCT(mat=matrix(rnorm(100), nrow=10),
           rid=letters[1:10], cid=LETTERS[1:10])
  expect_true(is(g, "GCT"))
  expect_equal(dim(g@mat), c(10, 10))
  expect_equal(nrow(g@rdesc), 0)
  expect_equal(nrow(g@cdesc), 0)
  # and with file path
  g <- GCT(src="test_n5x10.gct")
  expect_true(is(g, "GCT"))
  expect_equal(dim(g@mat), c(10, 5))
  # adding 1 to the number of columns to account for the 'id' column
  # that gets added during parsing
  expect_equal(dim(g@rdesc), c(10, 11))
  expect_equal(dim(g@cdesc), c(5, 43))
  # if mat and src are given should use mat
  g <- GCT(mat=matrix(rnorm(100), nrow=10),
           rid=letters[1:10], cid=LETTERS[1:10],
           src="test_n5x10.gct")
  expect_equal(dim(g@mat), c(10, 10))
})

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

