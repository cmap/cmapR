context("Testing math functions")

test_that("robust.zscore works properly", {
  x <- readRDS("rnorm20.rds")
  truth_rz <- readRDS("robust_zs_rnorm20.rds")
  test_rz <- cmapR::robust.zscore(x)
  expect_identical(test_rz, truth_rz)
})

test_that("distil works properly", {
  m <- readRDS("rnorm_matrix.rds")
  truth_distil <- readRDS("distil_result.rds")
  test_distil <- cmapR::distil(m)
  expect_identical(test_distil, truth_distil)
})