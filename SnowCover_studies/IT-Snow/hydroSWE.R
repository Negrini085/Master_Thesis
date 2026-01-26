# The main goal of this script is to be able to plot SWE evolution over a 
# hydrological year and to make a brief statistical analysis
rm(list = ls())
gc()

dat <- read.table("Datas/swe_evolution.dat")
swe <- dati$V1

# Cycle to divide SWE values into hydrological years (1 Sept -> 31 Aug)
years <- 2011:2025

