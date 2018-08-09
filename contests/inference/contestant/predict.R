# Predictions
args <- commandArgs(T)
if(length(args) != 4) {
	cat("usage: predict.r m1 m2 test out\n")
	q()
}
m1 <- readRDS(args[1])
m2 <- readRDS(args[2])
test <- readRDS(args[3])

library(parallel)
options(mc.cores = detectCores()-1)

cors <- mclapply(1:2, function(i){
	if(i == 1){
		return(cor(test, m1, method = 'pearson'))
	}else{
		return(cor(test, m1, method = 'spearman'))
	}
})

get.best <- function(cors, ng){
    best <- order(-cors)[1:ng]
    best.cor <- cors[best]
    best.cor[best.cor < 0] <- 0
    data.frame(best = best, wt = best.cor)
}

output <- do.call(cbind, mclapply(1:ncol(test), function(n){
    ng <- 20
    best <- rbind(
        get.best(cors[[1]][n, ], ng),
        get.best(cors[[2]][n, ], ng)
    )
    apply(m2[, best$best], 1, function(v) sum(v * best$wt)/sum(best$wt))
}))

outcsv <- sprintf("%s.csv", args[4])
outrds <- sprintf("%s.rds", args[4])
#write.table(round(output, 3), file = outcsv, row = F, col = F, sep = ',', quo = F)
saveRDS(output, outrds)
