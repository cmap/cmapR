context("testing GCT class methods")

test_that("invalid GCT slot assignments fail", {
  # try assigning a data.frame to the @mat slot
  expect_error({ foo@mat <- data.fame() })
  # try assigning a matrix to the rdesc and cdesc slots
  expect_error({ foo@cdesc <- matrix(nrow=1, ncol=1) })
  expect_error({ foo@rdesc <- matrix(nrow=1, ncol=1) })
  # try assigning a non-character value to rid and cid slots
  expect_error({ foo@rid <- seq(1, nrow(foo)) })
  expect_error({ foo@cid <- seq(1, ncol(foo)) })
  # try assigning a non-unique vector to rid and cid
  expect_error({
    foo <- cmapR::ds
    foo@rid <- rep("a", nrow(foo))
    validObject(foo)
    })
  expect_error({
    foo <- cmapR::ds
    foo@cid <- rep("a", ncol(foo))
    validObject(foo)
  })
  # try assigning rid/cid of length different from matrix dimensions
  expect_error({
    foo <- cmapR::ds
    foo@rid <- "a"
    validObject(foo)
  })
  expect_error({
    foo <- cmapR::ds
    foo@cid <- "a"
    validObject(foo)
  })
  # try assigning data.frames of incorrect dimenions
  expect_error({
    foo <- cmapR::ds
    foo@rdesc <- data.frame(x=1, y=1)
    validObject(foo)
  })
  expect_error({
    foo <- cmapR::ds
    foo@cdesc <- data.frame(x=1, y=1)
    validObject(foo)
  })
  # should be OK to assign emtpy data.frames though
  expect_true({
    foo <- cmapR::ds
    foo@rdesc <- data.frame()
    validObject(foo)
  })
  expect_true({
    foo <- cmapR::ds
    foo@cdesc <- data.frame()
    validObject(foo)
  })
})

test_that("generating an ad-hoc GCT works", {
  # generate a valid dataset
  expect_true({
    ds <- new("GCT", mat=matrix(rnorm(100), nrow=10), rid=letters[1:10], cid=LETTERS[1:10])
    validObject(ds)
  })
  # now make some invalid datasets
  # misalignment between rid/cid and matrix dims
  expect_error({
    ds <- new("GCT", mat=matrix(rnorm(100), nrow=10), rid=letters[1:5], cid=LETTERS[1:10])
    validObject(ds)
  })
  expect_error({
    ds <- new("GCT", mat=matrix(rnorm(100), nrow=10), rid=letters[1:10], cid=LETTERS[1:5])
    validObject(ds)
  })
  # misalign rdesc and cdesc dimensions
  expect_error({
    ds <- new("GCT", mat=matrix(rnorm(100), nrow=10), rid=letters[1:10], cid=LETTERS[1:10],
              rdesc=data.frame(x=1, y=1))
    validObject(ds)
  })
  expect_error({
    ds <- new("GCT", mat=matrix(rnorm(100), nrow=10), rid=letters[1:10], cid=LETTERS[1:10],
              cdesc=data.frame(x=1, y=1))
    validObject(ds)
  })
  # should fail if we provide an unnamed matrix and omit rid, cid
  m <- matrix(rnorm(100), nrow=10)
  expect_error({ ds <- new("GCT", mat=m) })
  # should be ok if we assign row/col names to m
  rownames(m) <- letters[1:10]
  colnames(m) <- LETTERS[1:10]
  expect_true({
    ds <- new("GCT", mat=m)
    validObject(m)
  })
})
