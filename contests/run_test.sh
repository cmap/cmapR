#!/bin/bash

# --- data preparation
echo "Preparing data..."
Rscript contestant/prepdata.R

# --- predictions
echo "Making predictions..."
Rscript contestant/predict.R out/m1.rds out/m2.rds out/test.rds out/x-xraw
Rscript contestant/predict.R out/combat.m1.rds out/m2.rds out/combat.test.rds out/x-combat
Rscript contestant/predict.R out/rank-m1.rds out/m2.rds out/rank-test.rds out/x-rank
Rscript contestant/predict.R out/scaled-m1.rds out/m2.rds out/scaled-test.rds out/x-scale
Rscript contestant/predict.R out/scaled-m1.rds out/m2.rds out/test.rds out/x-scale2
Rscript contestant/predict.R out/quantile.m1.rds out/m2.rds out/quantile.test.rds out/x-quantile

# --- ensemble 
echo "Computing the ensemble..."
Rscript contestant/ensemble.R out/final.csv

echo "...done!"