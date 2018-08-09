library(sva)
library(parallel)
library(cmapRinference)
source("R/inference.R")

# --- Prepare data
genes <- read.table("../data/GSE92743_Broad_GTEx_gene_info.txt", sep="\t", head=T, quote="", comment.char="")
rid_lmk <- as.character(genes$pr_gene_id)[genes$pr_is_lm==1]
rid_nonlmk <- as.character(genes$pr_gene_id)[genes$pr_is_lm==0]

landmark <- parse.gctx("../data/GSE92743_Broad_Affymetrix_training_Level3_Q2NORM_n100000x12320.gctx", cid=1:1000, rid=rid_lmk)
nonlandmark <- parse.gctx("../data/GSE92743_Broad_Affymetrix_training_Level3_Q2NORM_n100000x12320.gctx", cid=1:1000)

# L1000 samples (same cell coltures)
l1000 <- parse.gctx("../data/GSE92743_Broad_GTEx_L1000_Holdout_Level3_Q2NORM_n1000x12320.gctx", cid=1:100)
l1000.large <- parse.gctx("../data/GSE92743_Broad_GTEx_L1000_Holdout_Level3_Q2NORM_n1000x12320.gctx"
  , cid=1:1000)

# ground truth
gt <-  parse.gctx("../data/GSE92743_Broad_GTEx_RNAseq_Log2RPKM_q2norm_n3176x12320.gctx")

# metadata about samples (match l1000 with RNAseq)
samples <- read.table("../data/GSE92743_Broad_GTEx_inst_info.txt", sep="\t", head=T)
 

# --- function to simulate data 
simulate_nonlmk <- function(x, size, betas, ...) {
	t(betas) %*% x + matrix(rnorm(size * ncol(x), ...), size, ncol(x))
}
simulate_data <- function(samples, lmk, non_lmk, betas, ...) {
	landmarks <- matrix(rnorm(lmk*samples), lmk, samples)
	rownames(landmarks) <- paste0("lmk_id_", 1:nrow(landmarks))
	nonlandmarks <- simulate_nonlmk(landmarks, size=non_lmk, betas=betas, ...)
	rownames(nonlandmarks) <- paste0("nonlmk_id_", 1:nrow(nonlandmarks))
	rbind(landmarks, nonlandmarks)
}

# Training set
samples <- 10000
samples_target <- 100
non_lmk <- 200
lmk <- 50
betas <- matrix(rnorm(lmk*non_lmk, mean=10, sd=1), lmk, non_lmk)
d_train <- simulate_data(samples, lmk, non_lmk, betas) # training 
d_target <- simulate_data(samples_target, lmk, non_lmk, betas)			# testing
d_target_lmk <- d_target[grep("^lmk", rownames(d_target)), ]
d_target_nonlmk <- d_target[grep("^nonlmk", rownames(d_target)), ]

(out <- cmap_impute(d_train, d_target_lmk, K=10, type="none"))
(ensemble <- cmap_impute_ensemble(d_train, d_target_lmk, K=10))

rho <- cor(d_target_nonlmk, out$pred)
rho2 <- cor(d_target_nonlmk, ensemble$pred)
hist(rho,"Scott")
hist(rho2,"Scott", add=TRUE, col=2)
