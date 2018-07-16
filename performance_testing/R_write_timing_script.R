# '/path/to/large/gctx/file' refers to a large GCTX file (any size above 10174x100000 should work) from which file subsets are made.
# In testing, the large GCTX file used lacked metadata; including metadata would cause slight variation in results.
# Cache was cleared in between consecutive operations.

library(cmapR)

in_filenames = c()
gct_write_results = c()
gctx_write_results = c()

large_gctx = parse.gctx("/path/to/large/gctx/file")

col_spaces = c(96, 384, 1536, 3000, 6000, 12000, 24000, 48000, 100000)
row_spaces = c(978, 10174)

for (c in col_spaces) {
  for (r in row_spaces) {
    curr_gctoo = subset.gct(large_gctx, rid = 1:r, cid = 1:c)
    out_fname = paste("write_test_n", paste(c, r, sep="x"), sep="_")
    
    # gctx writing
    start = Sys.time()
    write.gctx(curr_gctoo, out_fname)
    end = Sys.time()
    gct_elapsed_time = end - start
    
    # gct writing
    start = Sys.time()
    write.gct(curr_gctoo, out_fname)
    end = Sys.time()
    gctx_elapsed_time = end - start
    
    in_filenames = c(in_filenames, paste(c, r, sep="x"))
    gct_write_results = c(gct_write_results, gct_elapsed_time)
    gctx_write_results = c(gctx_write_results, gctx_elapsed_time)
  }
}

write_results_df = as.data.frame(cbind(in_filenames, gct_write_results, gctx_write_results))
write.table(write_results_df, "R_writing_results.txt", sep="\t", quote=FALSE, row.names=FALSE)
