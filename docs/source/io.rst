IO (io.R)
=========

Generally, this module contains the GCT class and relevant method definitions. These are:

GCT Object (S4 class)
---------------------

.. highlight:: r

::
	
	#An S4 class to represent a GCT object
	
	@slot mat a numeric matrix
	@slot rid a character vector of row ids
	@slot cid a character vector of column ids
	@slot rdesc a \code{data.frame} of row descriptors
	@slot rdesc a \code{data.frame} of column descriptors
	@slot src a character indicating the source (usually file path) of the data
	
	@description The GCT class serves to represent annotated
	  matrices. The \code{mat} slot contains said data and the
	  \code{rdesc} and \code{cdesc} slots contain data frames with
	  annotations about the rows and columns, respectively
	  
	@seealso \code{\link{parse.gctx}}, \code{\link{write.gctx}}, \code{\link{read.gctx.meta}}, \code{\link{read.gctx.ids}}
	@seealso \link{http://clue.io/help} for more information on the GCT format

	setClass("GCT",
	         representation(
	             mat = "matrix",
	             rid = "character",
	             cid = "character",
	             rdesc = "data.frame",
	             cdesc = "data.frame",
	             version = "character",
	             src = "character"
	         )
	)


GCTX parsing functions
----------------------

**Parse a .gct or .gctx file to GCT object**

.. highlight:: r

::

	parse.gctx <- function(fname, rid=NULL, cid=NULL, set_annot_rownames=F, matrix_only=F) 
	
	@param fname path to the GCTX file on disk
	@param rid either a vector of character or integer
	  row indices or a path to a grp file containing character
	  row indices. Only these indicies will be parsed from the
	  file.
	@param cid either a vector of character or integer
	  column indices or a path to a grp file containing character
	  column indices. Only these indicies will be parsed from the
	  file.
	@param set_annot_rownames boolean indicating whether to set the
	  rownames on the row/column metadata data.frames. Set this to 
	  false if the GCTX file has duplicate row/column ids.
	@param matrix_only boolean indicating whether to parse only
	  the matrix (ignoring row and column annotations)

	@details \code{parse.gctx} also supports parsing of plain text
	  GCT files, so this function can be used as a general GCT parser.
	
	@examples 
	gct_file <- system.file("extdata", "modzs_n272x978.gctx", package="roller")
	(ds <- parse.gctx(gct_file))
	
	# matrix only
	(ds <- parse.gctx(gct_file, matrix_only=T))
	
	# only the first 10 rows and columns
	(ds <- parse.gctx(gct_file, rid=1:10, cid=1:10))
	
	@family GCTX parsing functions


**Parse row/column metadata only**

.. highlight:: r

::

	read.gctx.meta <- function(gctx_path, dimension="row", ids=NULL, set_annot_rownames=T)
	
	@param gctx_path the path to the GCTX file
	@param dimension which metadata to read (row or column)
	@param ids a character vector of a subset of row/column ids
	  for which to read the metadata
	@param set_annot_rownames a boolean indicating whether to set the 
	  \code{rownames} addtribute of the returned \code{data.frame} to
	  the corresponding row/column ids.
	 
	@return a \code{data.frame} of metadata
	
	@examples
	gct_file <- system.file("extdata", "modzs_n272x978.gctx", package="roller") 
	# row meta
	row_meta <- read.gctx.meta(gct_file)
	str(row_meta)
	# column meta
	col_meta <- read.gctx.meta(gct_file, dimension="column")
	str(col_meta)
	# now for only the first 10 ids
	col_meta_first10 <- read.gctx.meta(gct_file, dimension="column", ids=col_meta$id[1:10])
	str(col_meta_first10)
	
	@family GCTX parsing functions


**Parse row/column ids only**

.. highlight:: r

::
	
	read.gctx.ids <- function(gctx_path, dimension="row")

	#Read GCTX row or column ids
	
	@param gctx_path path to the GCTX file
	@param dimension which ids to read (row or column)
	
	@return a character vector of row or column ids from the provided file
	
	@examples 
	gct_file <- system.file("extdata", "modzs_n272x978.gctx", package="roller")
	# row ids
	rid <- read.gctx.ids(gct_file)
	head(rid)
	# column ids
	cid <- read.gctx.ids(gct_file, dimension="column")
	head(cid)
	
	@family GCTX parsing functions



GCTX writing functions
----------------------

**Write a GCT object to disk in .gct format**

.. highlight:: r 

::

	write.gct <- function(ds, ofile, precision=4, appenddim=T, ver=3)

	@param ds the GCT object
	@param ofile the desired output filename
	@param precision the numeric precision at which to
	  save the matrix. See \code{details}.
	@param appenddim boolean indicating whether to append
	  matrix dimensions to filename
	@param ver the GCT version to write. See \code{details}.
	
	@details Since GCT is text format, the higher \code{precision}
	  you choose, the larger the file size.
	  \code{ver} is assumed to be 3, aka GCT version 1.3, which supports
	  embedded row and column metadata in the GCT file. Any other value
	  passed to \code{ver} will result in a GCT version 1.2 file which
	  contains only the matrix data and no annotations.

	@return NULL
	
	@examples 
	\dontrun{
	write.gct(ds, "dataset", precision=2)
	}
	@family GCTX parsing functions


**Write a GCT object to disk in .gctx format**

.. highlight:: r

::

	write.gctx <- function(ds, ofile, appenddim=T, compression_level=0, matrix_only=F) 

	@param ds a GCT object
	@param ofile the desired file path for writing
	@param appenddim boolean indicating whether the
	  resulting filename will have dimensions appended
	  (e.g. my_file_n384x978.gctx)
	@param compression_level integer between 1-9 indicating
	  how much to compress data before writing. Higher values
	  result in smaller files but slower read times.
	@param matrix_only boolean indicating whether to write
	  only the matrix data (and skip row, column annotations)
	
	@examples 
	\dontrun{
	# assume ds is a GCT object
	write.gctx(ds, "my/desired/outpath/and/filename")
	}

	@family GCTX parsing functions

**Write a ``data.frame`` of metadata only to a GCTX file**

.. highlight:: r

::
	
	write.gctx.meta <- function(ofile, df, dimension="row")

	@param ofile the desired file path for writing
	@param df the \code{data.frame} of annotations
	@param dimension the dimension to annotate
	  (row or column)
	
	@examples 
	\dontrun{
	# assume ds is a GCT object
	write.gctx.meta("/my/file/path", cdesc_char, dimension="col")
	}
	@family GCTX parsing functions
	@keywords internal

Parsing GRP files
-----------------

**Parse a .grp file to vector**

.. highlight:: r 

::

	parse.grp <- function(fname)
	
	@param fname the file path to be parsed
	@return a vector of the contents of \code{fname}

	@examples 
	grp_path <- system.file("extdata", "lm_epsilon_n978.grp", package="roller")
	values <- parse.grp(grp_path)
	str(values)

	@family CMap parsing functions
	@seealso \link{http://clue.io/help} for details on the GRP file format
	
Writing to .grp files
---------------------

**Write a vector to a .grp file**

.. highlight:: r

::

	write.grp <- function(vals, fname)

	@param vals the vector of values to be written
	@param fname the desired file name
	
	@examples 
	\dontrun{
	write.grp(letters, "letter.grp")
	}
	
	@family CMap parsing functions
	@seealso \link{http://clue.io/help} for details on the GRP file format

Parsing GMX files
-----------------

**Parse a .gmx file to a list**

.. highlight:: r

:: 

	parse.gmx <- function(fname) 

	@param fname the file path to be parsed
	
	@return a list of the contents of \code{fname}. See details.
	
	@details \code{parse.gmx} returns a nested list object. The top
	  level contains one list per column in \code{fname}. Each of 
	  these is itself a list with the following fields:
	  - \code{head}: the name of the data (column in \code{fname})
	  - \code{desc}: description of the corresponding data
	  - \code{len}: the number of data items
	  - \code{entry}: a vector of the data items
	
	@examples 
	gmx_path <- system.file("extdata", "lm_probes.gmx", package="roller")
	gmx <- parse.gmx(gmx_path)
	str(gmx)
	
	@family CMap parsing functions
	@seealso \link{http://clue.io/help} for details on the GMX file format

Parsing GMT files
-----------------

**Parse a .gmt file to a list**

.. highlight:: r

:: 

	parse.gmt <- function(fname)

	@param fname the file path to be parsed
	
	@return a list of the contents of \code{fname}. See details.
	
	@details \code{parse.gmt} returns a nested list object. The top
	  level contains one list per row in \code{fname}. Each of 
	  these is itself a list with the following fields:
	  - \code{head}: the name of the data (row in \code{fname})
	  - \code{desc}: description of the corresponding data
	  - \code{len}: the number of data items
	  - \code{entry}: a vector of the data items
	
	@examples 
	gmt_path <- system.file("extdata", "query_up.gmt", package="roller")
	gmt <- parse.gmt(gmt_path)
	str(gmt)
	
	@family CMap parsing functions
	@seealso \link{http://clue.io/help} for details on the GMT file format

Writing to GMT files
--------------------

.. highlight:: r

::

	write.gmt <- function(lst, fname)

	@param lst the nested list to write. See \code{details}.
	@param fname the desired file name
	
	@details \code{lst} needs to be a nested list where each 
	  sub-list is itself a list with the following fields:
	  - \code{head}: the name of the data
	  - \code{desc}: description of the corresponding data
	  - \code{len}: the number of data items
	  - \code{entry}: a vector of the data items
	
	@examples 
	\dontrun{
	write.gmt(gene_set, "gene_set.gmt")
	}
	
	@family CMap parsing functions
	@seealso \link{http://clue.io/help} for details on the GMT file format

Writing a ``data.frame`` to a tsv file
--------------------------------------

.. highlight:: r

::

	write.tbl <- function(tbl, ofile, ...)

	@param tbl the \code{data.frame} to be written
	@param ofile the desired file name
	@param ... additional arguments passed on to \code{write.table}
	
	@details This method simply calls \code{write.table} with some
	  preset arguments that generate a unquoted, tab-delimited file
	  without row names.
	
	@examples 
	\dontrun{
	write.tbl(cdesc_char, "col_meta.txt")
	}
	
	@seealso \code{\link{write.table}}