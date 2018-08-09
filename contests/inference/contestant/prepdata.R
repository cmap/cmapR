#!/usr/bin/env Rscript
# This scripts simulates some data and implements the codes of the winning contestant

library(sva) # for ComBat()
library(parallel)
options(mc.cores = detectCores() - 1)

# dir.create("out") # for files
# 
# # --- simulate fake data and save them 
# samples <- 1000 		# samples
# lmrk <- 970 				# landmarks
# nonlmrk <- 1000 		# nonlandmarks
# samples_test <- 50 	# for prediction
# 
# # --- data 
# m1 <- matrix(rlnorm(lmrk * samples), ncol=samples, nrow=lmrk)
# m2 <- matrix(rlnorm(nonlmrk * samples), ncol=samples, nrow=nonlmrk)
# test <- matrix(rlnorm(lmrk * samples_test), ncol=samples_test, nrow=lmrk)
# 
# # --- save 
# saveRDS(m1, file = 'out/m1.rds', compress = F)
# saveRDS(m2, file = 'out/m2.rds', compress = F)
# saveRDS(test, file = 'out/test.rds')

# --- Combat normalization

m1 <- readRDS('out/m1.rds')
test <- readRDS('out/test.rds')

labels <- as.factor(c(rep('train', ncol(m1)), rep('test', ncol(test))))
mod0 <- matrix(1, nrow = length(labels), ncol = 1)
final <- ComBat(dat = cbind(m1, test), batch = labels, mod = mod0, par.prior=T)

train2 <- final[, 1:ncol(m1)]
test2 <- final[, (ncol(m1)+1):ncol(final)]

saveRDS(train2, file = 'out/combat.m1.rds', compress = F)
saveRDS(test2, file = 'out/combat.test.rds', compress = F)

# --- Scale normalization

m1 <- readRDS('out/m1.rds')
test <- readRDS('out/test.rds')

m1 <- t(scale(t(m1)))
test <- t(scale(t(test)))

saveRDS(m1, file = 'out/scaled-m1.rds', compress = F)
saveRDS(test, file = 'out/scaled-test.rds', compress = F)

# --- Rank normalization

m1 <- readRDS('out/m1.rds')
test <- readRDS('out/test.rds')

m1 <- t(apply(m1, 1, rank))
test <- t(apply(test, 1, rank))

saveRDS(m1, file = 'out/rank-m1.rds', compress = F)
saveRDS(test, file = 'out/rank-test.rds', compress = F)

# --- Quantile normalizaiton

m1 <- readRDS('out/m1.rds')
test <- readRDS('out/test.rds')

normalize.quantiles <- function(m){
    ranked1 <- apply(m, 2, rank, ties.method="min")
    ranked2 <- apply(m, 2, rank, ties.method="max")
    sorted <- apply(m, 2, sort)
    meaned <- apply(sorted, 1, mean)
    nr <- nrow(m)
    do.call(cbind, mclapply(1:ncol(m), function(i){
        sapply(1:nr, function(j){
            mean(meaned[ranked1[j,i]:ranked2[j,i]])
        })
    }))
}

m1 <- normalize.quantiles(m1)
test <- normalize.quantiles(test)

library(sva)
labels <- as.factor(c(rep('train', ncol(m1)), rep('test', ncol(test))))
mod0 <- matrix(1, nrow = length(labels), ncol = 1)
final <- ComBat(dat = cbind(m1, test), batch = labels, mod = mod0, par.prior=T, prior.plot = T)

train2 <- final[, 1:ncol(m1)]
test2 <- final[, (ncol(m1)+1):ncol(final)]

saveRDS(train2, file = 'out/quantile.m1.rds', compress = F)
saveRDS(test2, file = 'out/quantile.test.rds', compress = F)


