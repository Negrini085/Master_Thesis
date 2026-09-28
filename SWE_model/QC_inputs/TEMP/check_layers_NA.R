# The main goal of this script is to check weather a single map can be made only
# of NAs
rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")
years <- 1945:2023

for(y in years){
  fname <- paste0("../../Dataset/TEMP/", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No temperature file for ", y))
  
  # Opening precipitation maps
  nc <- nc_open(fname)
  tmxd <- ncvar_get(nc,"tmxd")
  tmd <- ncvar_get(nc,"tmd")
  tmnd <- ncvar_get(nc,"tmnd")
  
  for(i in 1:dim(tmxd)[3]){
    daily_map <- tmxd[, , i]
    if(all(is.na(daily_map))) stop(paste0("All tmxd datapoints are NAs during ", i, " day of ", y))
  }
  
  for(i in 1:dim(tmnd)[3]){
    daily_map <- tmnd[, , i]
    if(all(is.na(daily_map))) stop(paste0("All tmnd datapoints are NAs during ", i, " day of ", y))
  }
  
  for(i in 1:dim(tmd)[3]){
    daily_map <- tmd[, , i]
    if(all(is.na(daily_map))) stop(paste0("All tmd datapoints are NAs during ", i, " day of ", y))
  }
  
  nc_close(nc)
  rm(tmxd)
  rm(tmnd)
  rm(tmd)
  rm(nc)
  gc()
}