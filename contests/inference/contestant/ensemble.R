########################################
# Final (parallel)
#
# 	inputs:
# 		m1, m2, test
#
# 	outputs:
# 		scaled-m1, scaled-test		
########################################

# Final
args <- commandArgs(T)
raw <- readRDS('out/x-xraw.rds')
combat <- readRDS('out/x-combat.rds')
ranked <- readRDS('out/x-rank.rds')
scaled2 <- readRDS('out/x-scale2.rds')
scaled <- readRDS('out/x-scale.rds')
quantile <- readRDS('out/x-quantile.rds')

avg <- 0.3 * raw + 0.3 * combat + 0.2 * scaled2 + 
	0.2/3 * ranked +
	0.2/3 * scaled +
	0.2/3 * quantile

write.table(round(avg, 3), file = args[1], row = F, col = F, sep = ',', quo = F)

