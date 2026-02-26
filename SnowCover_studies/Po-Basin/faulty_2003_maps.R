# The main goal of this script is to find faulty days during 2003 season. I would
# like to select a specific location and then analyze its time series. How many NAs
# there are? Is it something I can deal with or not?
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")