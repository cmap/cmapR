utils (utils.R)
===============

Melting GCT objects
-------------------

Transform a GCT object to long form (aka 'melt'). 

.. highlight:: r

::

	melt.gct(g, suffixes=NULL, remove_symmetries=F, keep_rdesc=T, keep_cdesc=T) 

	@description Utilizes the \code{\link{data.table::melt}} function to transform the
	  matrix into long form. Optionally can include the row and column
	  annotations in the transformed \code{\link{data.table}}.
	  
	@param g the GCT object
	@param keep_rdesc boolean indicating whether to keep the row
	  descriptors in the final result
	@param keep_cdesc boolean indicating whether to keep the column
	  descriptors in the final result
	@param remove_symmetries boolean indicating whether to remove
	  the lower triangle of the matrix (only applies if \code{g@mat} is symmetric)
	@param suffixes the character suffixes to be applied if there are
	  collisions between the names of the row and column descriptors
	  
	@return a \code{\link{data.table}} object with the row and column ids and the matrix
	  values and (optionally) the row and column descriptors
	  
	@examples 
	# simple melt, keeping both row and column meta
	head(melt.gct(ds))
	
	# update row/colum suffixes to indicate rows are genes, columns experiments
	head(melt.gct(ds, suffixes = c("_gene", "_experiment")))
	
	# ignore row/column meta
	head(melt.gct(ds, keep_rdesc = F, keep_cdesc = F))
	
	@family GCT utilities
	

Concatenating
-------------

**Merge two GCT objects**

.. highlight:: r

::

	merge.gct(g1, g2, dimension="row", matrix_only=F)

	@param g1 the first GCT object
	@param g2 the second GCT object
	@param dimension the dimension on which to merge (row or column)
	@param matrix_only boolean idicating whether to keep only the
	  data matrices from \code{g1} and \code{g2} and ignore their
	  row and column meta data
	
	@examples
	# take the first 10 and last 10 rows of an object
	# and merge them back together
	(a <- subset.gct(ds, rid=1:10))
	(b <- subset.gct(ds, rid=969:978))
	(merged <- merge.gct(a, b, dimension="row"))
	
	@family GCT utilities
	@export


Slicing
-------

**Slice a GCT object using the provided row and/or column ids**

.. highlight:: r

::

	subset.gct(g, rid=NULL, cid=NULL)

	@param g a gct object
	@param rid a vector of character ids or integer indices for ROWS
	@param cid a vector of character ids or integer indices for COLUMNS

	@examples
	# first 10 rows and columns by index
	(a <- subset.gct(ds, rid=1:10, cid=1:10))
	
	# first 10 rows and columns using character ids
	(b <- subset.gct(ds, rid=ds@rid[1:10], cid=ds@cid[1:10]))
	
	identical(a, b) # TRUE
	
	@family GCT utilities


Annotating
----------

Given a GCT object and either a ``data.frame`` or a path to an annotation table, apply the annotations to the GCT using the given ``keyfield``. 

.. highlight:: r

:: 

	annotate.gct(g, annot, dimension="row", keyfield="id")

	@description Given a GCT object and either a \code{\link{data.frame}} or
	a path to an annotation table, apply the annotations to the
	gct using the given \code{keyfield}.
	
	@param g a GCT object
	@param annot a \code{\link{data.frame}} or path to text table of annotations
	@param dimension either 'row' or 'column' indicating which dimension
	  of \code{g} to annotate
	@param keyfield the character name of the column in \code{annot} that 
	  matches the row or column identifiers in \code{g}
	  
	@return a GCT object with annotations applied to the specified
	  dimension
	  
	@examples 
	\dontrun{
	 g <- parse.gctx('/path/to/gct/file')
	 g <- annotate.gct(g, '/path/to/annot')
	}
	
	@family GCT utilities


Transpose
---------

.. highlight:: r

::

	transpose.gct(g)

	@param g the \code{GCT} object
	
	@return a modified verion of the input \code{GCT} object
	  where the matrix has been transposed and the row and column
	  ids and annotations have been swapped.
	  
	@examples 
	transpose.gct(ds)
	
	@family GCT utilties
	@export


Math
----


**Convert values in a matrix to ranks**

.. highlight:: r

::

	rank.gct(g, dim="row")
	@param g the \code{GCT} object to rank
	@param dim the dimension along which to rank
	  (row or column)
	
	@return a modified version of \code{g}, with the
	  values in the matrix converted to ranks
	  
	@examples 
	(ranked <- rank.gct(ds, dim="column"))
	# scatter rank vs. score for a few columns
	plot(ds@mat[, 1:3], ranked@mat[, 1:3],
	  xlab="score", ylab="rank")
	
	@family GCT utilities
