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


# How many layers are there? There are 22645 layers, which makes sense because
# 365 days x 62 years makes 22630 days plus 15 leap years. Only one thing doesn't
# make sense, namely being that the dates starts with 22523.
time <- ncvar_get(nc, "time")
print(length(time))
print(time[1])


# I now want to check SWECLQMD properties in order to understand how to deal with 
# those kind of datas. I am now sure that those maps are daily snapshots of SWE.
ncatt_get(nc, "SWECLQMD")
swe <- ncvar_get(nc, "SWECLQMD")
print(dim(swe))
rm(swe)
gc()


# Last thing I have to deal with are latitude and longitude coordinates, in order
# to be able to compute SWE total volume and its evolution throughout the whole period.
# It looks like those pixels are exactely a 1000 meter x 1000 meter
lat <- ncvar_get(nc, "N")
lon <- ncvar_get(nc, "E")
print(lat)
print(lon)

nc_close(nc)