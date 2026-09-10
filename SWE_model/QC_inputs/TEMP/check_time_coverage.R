# The main goal of this script is to check weather some daily maps are missing or
# something like that.
rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")
years <- 1951:2023

for(y in years){
  fname <- paste0("../../Dataset/TEMP/", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No temperature file for ", y))
  
  # Opening precipitation file
  nc <- nc_open(fname)
  tmax <- ncvar_get(nc, "tmxd")
  
  # Selecting year length
  len <- 365
  if(y %% 4 == 0) len <- 366
  
  # Checking length
  if(len != dim(tmax)[3]) stop(paste0("No correct amount of days for", y))
  rm(tmax)
  rm(nc)
  gc()
}