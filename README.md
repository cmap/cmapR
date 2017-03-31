# cmapR (CMap R code)

Parsing and utility functions for analyzing CMap data. To learn more about the CMap project at the Broad Institute, please visit [clue.io](https://clue.io).

### Install instructions

Dependencies are listed in `DESCRIPTION`

**Installing from source**

The easiest way to install the `cmapR` repository is to point your R's `install.packages` function at a tarball of the `cmapR` archive. You can generate this archive by cloning this repository and doing the following:

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
	
