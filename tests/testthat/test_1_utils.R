context("Testing utility functions")

test_that("melt.gct works properly", {
  mlt <- melt.gct(ds)
  expect_equal(nrow(mlt), nrow(ds@mat) * ncol(ds@mat))
})