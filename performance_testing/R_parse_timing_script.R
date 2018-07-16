# Note. '/path/with/gctx/files/to/test/*gct*' refers to a directory of GCT and/or GCTx files to time parsing operations on.

library(cmapR)

# for storing timing results
input_filenames <- c()
parse_times <-c()

# get all GCT and/or GCTx files in directory 
input_files <- list.files(path="/path/with/gctx/files/to/test", 
                          pattern="*.gct*", 
                          full.names=TRUE,
                          recursive=FALSE)

for (f in input_files) {
  start_time <- Sys.time()
  in_gctoo <- parse.gctx(f)
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  input_filenames <- c(input_filenames, f)
  parse_times <- c(parse_times, elapsed_time)
}

parse_timing_df <- as.data.frame(cbind(input_filenames, parse_times))
write.table(parse_timing_df, "R_parsing_results.txt", sep="\t", quote=FALSE, row.names = FALSE)
