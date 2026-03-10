# The main goal of this script is to inspect Alpine 3D recent dataset.
rm(list = ls())
gc()

library(ncdf4)

fname <- "Dataset/SWE_1962_2023.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")


# Importing dataset and printing dimension names and stuffs like that
nc <- nc_open(fname)
print(names(nc$var))
print(names(nc$dim))

# How many layers are there?
time <- ncvar_get()


# swe <- ncvar_get(nc, "swe")
# time <- ncvar_get(nc, "time")
# ncatt_get(nc, "swe")
# ncatt_get(nc, "smro")
# 
# 
# # SWE matrix dimensions
# print(paste0("SWE matrix dimensions: ", dim(swe)))
# 
# 
# # Focusing on coordinates
# print(nc$dim$easting)
# print(nc$dim$northing)
nc_close(nc)