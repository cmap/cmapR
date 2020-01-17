# cmapR (CMap R code)

## NOTICE - In submission to Bioconductor

We're in the process of submitting to Bioconductor and have changed the some of the function naming conventions to comply with their standards. Function names that used to contain `.` have been replaced with `_`. Hence, `parse.gctx` is now `parse_gctx` and so on. Please try that and see if it works. There is additional info and examples in the vignettes/tutorial.Rmd.




Parsing and utility functions for analyzing CMap data. To learn more about the CMap project at the Broad Institute, please visit [clue.io](https://clue.io).

### Install instructions

Dependencies are listed in `DESCRIPTION`

**Docker**

A docker container with cmapR can be obtained here: https://hub.docker.com/r/cmap/cmapr. This may be preferable for those who would like to use the package without installing on their system.

**Installing from source**

Perhaps the simplest way is to install directly from github using `devtools::install_github("cmap/cmapR")`. Note that this requires having previously installed the `devtools` package.

The script `install_cmapR.R` takes care of installing all the dependencies and then running `devtools::install_github("cmap/cmapR")`, so you can simply source this script after cloning this repository.

Alternatively, you can point your R's `install.packages` function at a tarball of the `cmapR` archive. You can generate this archive by cloning this repository and doing the following:

	# make a gzip tar ball of the repo
	R CMD build cmapR
	# makes cmapR_1.0.tar.gz
	
	# check that the package is ok
	R CMD check cmapR_1.0.tar.gz	

Once you have created the tarball, open an R terminal and execute the following:

	install.packages("cmapR_1.0.tar.gz", type="source", repos=NULL)
	library("cmapR")


You can also source individual files as needed instead of installing the entire package.

	# For example, just load the IO methods
	source("cmapR/R/io.R")

### Citation information
If you use GCTx and/or cmapR in your work, please cite [Enache et al.](https://www.biorxiv.org/content/early/2017/11/30/227041)
