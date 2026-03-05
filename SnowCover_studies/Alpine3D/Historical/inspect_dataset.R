# The main goal of this script is to inspect Alpine 3D recent dataset.
rm(list = ls())
gc()

library(ncdf4)

fname <- "Dataset/2016/OSHD_DATA_2015-09.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Recent/")


# Importing dataset
nc <- nc_open(fname)
swe <- ncvar_get(nc, "swe")
time <- ncvar_get(nc, "time")
ncatt_get(nc, "swe")
ncatt_get(nc, "smro")
nc_close(nc)


# Dimension names and stuffs like that
print(names(nc$var))
print(names(nc$dim))


# SWE matrix dimensions
print(paste0("SWE matrix dimensions: ", dim(swe)))


# Focusing on coordinates
print(nc$dim$easting)
print(nc$dim$northing)