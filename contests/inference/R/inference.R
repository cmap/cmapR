#' Impute cmap data using K-Nearest-Neighbors regression under various normalizations
#'
#' @param train a matrix in which we look for the nearest neighbors.
#' @param target a matrix in which we look for samples to predict.
#' @param K number of nearest neighbors.
#' @param train_row_id a vector of strings corresponding to the rows of the training matrix 
#'					in which we look for common names between train and target (default rownames)
#' @param target_row_id a vector of strings corresponding to the rows of the targeting matrix 
#'					in which we look for common names between train and target (default rownames)
#' @param cormat optionally, a list of correlation matrices between 
#'				the common rows of the train and target matrices.
#' @param type type of normalization which can be `combat`, `quantile`, `rank`, `scale` or `none` (the default).
#' @param ... other arguments
#'
#' @return list with predictions, normalized inputs, correlation matrices, 
#'          and other parameters
#'
#' @import sva
#' @import parallel
#' @import cmapR
#'
#' @rdname cmap_impute
#' @export cmap_impute
cmap_impute <- function(train, target, rid, K
	, type=c("none", "combat", "quantile", "rank", "scale") 
	, cormat=NULL, normalize_target=TRUE
	, ...) {
	# ds must be GCT objects
  stopifnot(class(train)=="GCT",class(target)=="GCT")
	# landmark ids can either be a vector of character strings corresponding
	# to row / column ids in the gct object, or integer vectors
	# corresponding to row / column indices
	if (is.numeric(rid)) rid <- train@rid[rid]
	stopifnot(all(rid %in% train@rid), all(rid %in% target@rid))
	# use matrices for calculations
	m1 <- subset.gct(train, rid=rid)@mat
	m2 <- subset.gct(train, rid=setdiff(train@rid, rid))@mat
	mtarget <- subset.gct(target, rid=rid)@mat	
	# normalize matrices according to input type
	type <- match.arg(type)
	if (type=="combat") {
		batch <- as.factor(c(rep('train', ncol(m1)), rep('mtarget', ncol(mtarget))))
		temp <- matrix.adj(cbind(m1, mtarget), "combat", batch)
		m1 <- temp[, 1:ncol(m1)]
		if (normalize_target) mtarget <- temp[, (ncol(m1)+1):ncol(temp)]
	} else {
		m1 <- matrix.adj(m1, type)
		if (normalize_target) mtarget <- matrix.adj(mtarget, type)
	}
	# if type = quantile, make additional "combat batch" normalization 
	if (type=="quantile") {
		batch <- as.factor(c(rep('train', ncol(m1)), rep('mtarget', ncol(mtarget))))
		temp <- matrix.adj(cbind(m1, mtarget), "combat", batch)
		m1 <- temp[, 1:ncol(m1)]
		if (normalize_target) mtarget <- temp[, (ncol(m1)+1):ncol(temp)]	
	}
	# Now, we run the prediction step and return a list with all the results
	pred <- cmap_impute_knn(m1, m2, mtarget, ng=K, cormat, ...)
	class(pred) <- "cmap_impute"
	pred$type <- type
	pred$normalize_target <- normalize_target
	pred$m1 <- m1
	pred$m2 <- m2
	pred$mtarget <- mtarget
	return(pred)
}

#' Obtain predictions from the K nearest neighbors
cmap_impute_knn <- function(m1, m2, target, ng, cormat, ...) {
	stopifnot(ncol(m1)==ncol(m2), nrow(m1)==nrow(target))
	# compute list of correlation matrices, if not provided
	if (is.null(cormat)) {
		cormat <- list()
		cormat$spearman <- cor(target, m1, method="spearman")
		cormat$pearson 	<- cor(target, m1, method="pearson")
	}
	# For each column in target matrix, find the ng nearest columns 
	# in each correlation matrix of the cormat list; then extract 
	# column position and corresponding correlation coefficient; 
	# use these values for m2 matrix to compute weighted ave. 
	# of the ng best neighbors
	pred <- lapply(1:ncol(target), function(n) {
		best <- lapply(cormat, function(x) get.best(x[n, ], ng))
		best <- do.call(rbind, best)
	  apply(m2[, best$best], 1, function(v) sum(v * best$wt)/sum(best$wt))
	})
	# return outcomes
	out <- list()
	out$pred <- do.call(cbind, pred)
	out$cormat <- cormat 
	out$ng <- ng
	return(out)
}

#
cmap_impute_ensemble <- function(train, target, rid, K=20) {
	out <- list()
	out$raw <- cmap_impute(train, target, rid, K=K, type="none")
	out$rank <- cmap_impute(train, target, rid, K=K, type="rank")
	out$scale <- cmap_impute(train, target, rid, K=K, type="scale")
	out$scale2 <- cmap_impute(train, target, rid, K=K, type="scale", normalize_target=FALSE)
	out$combat <- cmap_impute(train, target, rid, K=K, type="combat")
	out$quantile <- cmap_impute(train, target, rid, K=K, type="quantile")
	b <- lapply(out, function(x) x$pred)
	pred <- list(pred=0.3 * b$raw + 
		0.3 * b$combat + 
		0.2 * b$scale2 + 
		0.2/3 * b$rank + 
		0.2/3 * b$scale + 
		0.2/3 * b$quantile, ng=K)
# 	pred$train <- train
# 	pred$target <- target
	pred$type <- "ensemble"
	pred$cormat <- unlist(lapply(out, function(x) x$cormat), recursive=FALSE)
	class(pred) <- "cmap_impute"
	return(pred)
}
 
#' Add cmap_impute method to print function
print.cmap_impute <- function(x, ...) {
	cat("Cmap inference using K nearest neighbors\n\n")
	cat("Data normalization:", x$type,"\n\n")
	cat("Number of nearest neighbors:", x$ng, "\n\n")
	cat("Correlation quantiles by method:\n")
	print(sapply(x$cormat, quantile))
	cat("\nPrediction quantiles:\n")
	print(quantile(x$pred))
}

#' Adjust matrix for batch effects
#' 
#' @param x matrix to be adjusted
#' @param y matrix to be adjusted
#' @param ... additional options for normalization
#' @return If `y` is null, then the output will be a matrix normalized 
#' 				according to the selected \code{type}. If `y` is a matrix, then the output will 
#' 				be a list with both `x` and `y` matrices normalized.
matrix.adj <- function(x, type=c("none", "combat", "quantile", "rank", "scale")
	, batch=NULL, ...) {
  type <- match.arg(type)
	switch(type
  	, none = x
  	, combat = matrix.adj.combat(x, batch, ...)
  	, quantile = matrix.adj.quantile(x, ...)
  	, rank = t(apply(x, 1, rank, ...))
  	, scale = t(scale(t(x), ...))
  	)
}

#' Adjust ComBat
#' @import sva
matrix.adj.combat <- function(x, batch, par.prior=TRUE, ...) {
	mod0 <- matrix(1, nrow=length(batch), ncol=1)
	suppressMessages(ComBat(dat=x, batch=batch, mod=mod0, par.prior=par.prior, ...))
}

#' Adjust quantile
#' @import parallel
matrix.adj.quantile <- function(m, ...) {
    ranked1 <- apply(m, 2, rank, ties.method="min")
    ranked2 <- apply(m, 2, rank, ties.method="max")
    sorted <- apply(m, 2, sort)
    meaned <- apply(sorted, 1, mean)
    nr <- nrow(m)
    do.call(cbind, mclapply(1:ncol(m), function(i) {
        sapply(1:nr, function(j) {
            mean(meaned[ranked1[j,i]:ranked2[j,i]])
        })
    }, ...))
}


#' Get nearest neighbors `ng`
get.best <- function(cors, ng) {
    best <- order(-cors)[1:ng]
    best.cor <- cors[best]
    best.cor[best.cor < 0] <- 0
    data.frame(best = best, wt = best.cor)
}