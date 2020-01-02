context("Testing utility functions")

test_that("melt.gct works properly", {
  # standard
  ds <- cmapR::ds
  mlt <- melt.gct(ds)
  expect_equal(nrow(mlt), nrow(ds@mat) * ncol(ds@mat))
  expect_equal(ncol(mlt), ncol(ds@rdesc) + ncol(ds@cdesc) + 1)
  # ignore row/col annots
  mlt <- melt.gct(ds, keep_rdesc = FALSE, keep_cdesc = FALSE)
  expect_equal(nrow(mlt), nrow(ds@mat) * ncol(ds@mat))
  expect_equal(ncol(mlt), 3)
  # handle case where rdesc and cdesc are empty
  ds@rdesc <- data.frame()
  ds@cdesc <- data.frame()
  mlt <- melt.gct(ds)
  expect_equal(nrow(mlt), nrow(ds@mat) * ncol(ds@mat))
  expect_equal(ncol(mlt), 3)
})

test_that("merge.gct works properly", {
  # set up some test data
  ds1 <- parse.gctx("test_n5x10.gctx")
  ds2 <- ds1
  
  # scramble the rows and columns of ds2
  ridx <- sample(1:nrow(ds2@mat), nrow(ds2@mat))
  while(identical(ridx, 1:nrow(ds2@mat))) {
    ridx <- sample(1:nrow(ds2@mat), nrow(ds2@mat))
  }
  cidx <- sample(1:ncol(ds2@mat), ncol(ds2@mat))
  while(identical(cidx, 1:ncol(ds2@mat))) {
    cidx <- sample(1:ncol(ds2@mat), ncol(ds2@mat))
  }
  ds2@rid <- ds2@rid[ridx]
  ds2@rdesc <- ds2@rdesc[ridx, ]
  ds2@cid <- ds2@cid[cidx]
  ds2@cdesc <- ds2@cdesc[cidx, ]
  ds2@mat <- ds2@mat[ridx, cidx]
  
  # make sure scrambling worked
  expect_false(identical(ds1@rid, ds2@rid))
  expect_false(identical(ds1@cid, ds2@cid))
  
  ## CHECK ROW MERGING ##
  ds2@rid <- rownames(ds2@mat) <- ds2@rdesc$id <- paste("ds2", ds2@rid, sep=":")
  mrg <- merge.gct(ds1, ds2, dimension="row")
  
  # check the matrix
  # should have sum of 2 matrix rows
  # and same number of columns as first
  # should also have the same column names as the first
  expect_equal(nrow(mrg@mat), nrow(ds1@mat) + nrow(ds2@mat))
  expect_equal(ncol(mrg@mat), ncol(ds1@mat))
  expect_identical(colnames(mrg@mat), colnames(ds1@mat))
  
  # check the row ids
  # rid should be 2x as long as the 1st matrix
  expect_equal(length(mrg@rid), length(ds1@rid) + length(ds2@rid))
  
  # check the column ids
  # should have the same length as 1st matrix
  # and values should be identical
  # and sorted in the same order
  expect_identical(mrg@cid, ds1@cid)
  
  # check the row annots
  # should have sum of 2 matrix rows
  # and the same number of columns as the first
  expect_equal(nrow(mrg@rdesc), nrow(ds1@rdesc) + nrow(ds2@rdesc))
  expect_equal(ncol(mrg@rdesc), ncol(ds1@rdesc))
  
  # check the column annots
  # should have same number of rows and columns 
  # as the first matrix and in the same order
  expect_identical(mrg@cdesc, ds1@cdesc)
  
  ## CHECK COLUMN MERGING ##
  ds2@rid <- rownames(ds2@mat) <- ds2@rdesc$id <- gsub("ds2:", "", ds2@rid)
  ds2@cid <- colnames(ds2@mat) <- ds2@cdesc$id <- paste("ds2", ds2@cid, sep=":")
  mrg <- merge.gct(ds1, ds2, dimension="col")
  
  # check the matrix
  # should have sum of 2 matrix columns
  # and same number of rows as first
  # should also have the same row names as the first
  expect_equal(ncol(mrg@mat), ncol(ds1@mat) + ncol(ds2@mat))
  expect_equal(nrow(mrg@mat), nrow(ds1@mat))
  expect_identical(rownames(mrg@mat), rownames(ds1@mat))
  
  # check the row ids
  # should have the same length as 1st matrix
  # and values should be identical
  # and sorted in the same order
  expect_identical(mrg@rid, ds1@rid)
  
  # check the column ids
  # cid should be 2x as long as the 1st matrix
  expect_equal(length(mrg@cid), length(ds1@cid) + length(ds2@cid))
  
  # check the row annots
  # should have same number of rows and columns 
  # as the first matrix and in the same order
  expect_identical(mrg@rdesc, ds1@rdesc)
  
  # check the column annots
  # should have sum of 2 matrix rows
  # and the same number of columns as the first
  expect_equal(nrow(mrg@cdesc), nrow(ds1@cdesc) + nrow(ds2@cdesc))
  expect_equal(ncol(mrg@cdesc), ncol(ds1@cdesc))
  
  # add an explicit check to make sure the column
  # orderings haven't gone out of whack
  # correlations should all be 1
  common_rows <- intersect(ds1@rid, ds2@rid)
  ds1_corrs <- sapply(ds1@cid, function(x) {
    this_cor <- cor(ds1@mat[common_rows, x], mrg@mat[common_rows, x])
    this_cor
  })
  expect_true(all(round(ds1_corrs, 5) == 1))
  # same for 2nd ds
  ds2_corrs <- sapply(ds2@cid, function(x) {
    this_cor <- cor(ds2@mat[common_rows, x], mrg@mat[common_rows, x])
    this_cor
  })
  expect_true(all(round(ds2_corrs, 5) == 1))
  
  # check that annotations and matrix rows and columns are
  # in sync. this should hold regardless of the
  # dimension that was merged
  expect_identical(mrg@rdesc$id, rownames(mrg@mat))
  expect_identical(rownames(mrg@mat), mrg@rid)
  expect_identical(mrg@rdesc$id, mrg@rid)
  expect_identical(mrg@cdesc$id, colnames(mrg@mat))
  expect_identical(colnames(mrg@mat), mrg@cid)
  expect_identical(mrg@cdesc$id, mrg@cid)
  
})

test_that("subset.gct works properly", {
  ds <- cmapR::ds
  a <- subset.gct(ds, rid=1:10, cid=1:10)
  b <- subset.gct(ds, rid=ds@rid[1:10], cid=ds@cid[1:10])
  expect_identical(a, b)
})

test_that("annotate.gct works properly", {
  ds <- cmapR::ds
  newds <- ds
  col_meta <- ds@cdesc
  newds@cdesc <- data.frame(id=ds@cid)
  newds <- annotate.gct(newds, col_meta, dim="column", keyfield="id")
  # rearrange column order to match
  newds@cdesc <- newds@cdesc[, names(ds@cdesc)]
  # ignore row names
  rownames(ds@cdesc) <- NULL
  rownames(newds@cdesc) <- NULL
  expect_identical(newds, ds)
})

test_that("transpose.gct works properly", {
  ds <- cmapR::ds
  dst <- transpose.gct(ds)
  expect_identical(ds@mat, t(dst@mat))
  expect_identical(ds@cdesc, dst@rdesc)
  expect_identical(ds@rdesc, dst@cdesc)
  expect_identical(ds@cid, dst@rid)
  expect_identical(ds@rid, dst@cid)
})

test_that("rank.gct works properly", {
  ds <- cmapR::ds
  ranked_row <- rank.gct(ds, dim="row")
  expect_identical(range(ranked_row@mat), c(1, ncol(ds@mat)))
  ranked_col <- rank.gct(ds, dim="column")
  expect_identical(range(ranked_col@mat), c(1, nrow(ds@mat)))
  # ranked data should be completely anti-correlated with
  # scores if we use spearman. all correlations should be -1
  expect_equal(unname(diag(cor(ds@mat, ranked_col@mat, method="spearman"))),
               rep(-1, ncol(ds@mat)))
  ranked_col_inc <- rank.gct(ds, dim="column", decreasing=F)
  # ranked increasing data should be completely correlated with
  # scores if we use spearman. all correlations should be -1
  expect_equal(unname(diag(cor(ds@mat, ranked_col_inc@mat, method="spearman"))),
               rep(1, ncol(ds@mat)))
})

test_that("check_dups works properly", {
  foo <- c("a", "b", "c", "a")
  expect_error(check_dups(foo))
})

test_that("na_pad_matrix works properly", {
  m <- matrix(1, nrow=3, ncol=2)
  rownames(m) <- as.character(1:3)
  colnames(m) <- c("a", "b")
  padded <- na_pad_matrix(m, row_universe=as.character(1:5),
                          col_universe=letters[1:4])
  expect_equal(nrow(padded), 5)
  expect_equal(ncol(padded), 4)
  expect_true(all(is.na(padded[4:5, ])))
  expect_true(all(is.na(padded[, 3:4])))
  expect_false(any(is.na(padded[1:3, 1:2])))
})

test_that("align_matrices works properly", {
  # read ground truth 3D array
  arr3d <- readRDS("arr3d.rds")
  # try to construct the same thing using
  # align_matrices
  matrices <- readRDS("matrices.rds")
  res <- align_matrices(L=matrices)
  expect_identical(res, arr3d)
})

test_that("extract.gct works properly", {
  # read ground truth result
  truth_res <- readRDS("extract.gct.res.rds")
  # try to construct the same thing using
  # extract.gct
  test_res <- extract.gct(cmapR::kd_gct, row_field="pr_gene_symbol",
                          col_field="pert_mfc_desc")
  expect_equal(truth_res, test_res)
})