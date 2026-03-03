# The main goal of this script is to inspect SWE_1962_2023.nc content, in order
# to start analizing it. I feel like that this could filly 32G RAM.
rm(list = ls())
gc()

library(ncdf4)

fname <- "Dataset/SWE_1962_2023.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")


# Importing dataset
nc <- nc_open(fname)
time <- ncvar_get(nc, "time")
print(max(time))
print(names(nc$var))
print(names(nc$dim))
nc_close(nc)