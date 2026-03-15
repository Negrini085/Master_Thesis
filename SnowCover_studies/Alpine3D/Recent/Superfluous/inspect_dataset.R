# The main goal of this script is to inspect SWE_1962_2023.nc content, in order
# to start analizing it. I feel like that this could filly 32G RAM.
rm(list = ls())
gc()

library(ncdf4)

fname <- "Dataset/2016/OSHD_DATA_2015-09.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Recent/")


# Importing dataset
nc <- nc_open(fname)
time <- ncvar_get(nc, "time")
swe <- ncvar_get(nc, "swe")
print(dim(swe))
print(max(time))
print(names(nc$var))
print(names(nc$dim))
nc_close(nc)