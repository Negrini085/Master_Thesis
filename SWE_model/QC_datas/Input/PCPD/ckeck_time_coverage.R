# The main goal of this script is to check weather some daily maps are missing or
# something like that.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Input/PCPD/")
years <- 1951:2023

for(y in years){
  fname <- paste0("../../../Input/PCPD/", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No precipitation file for ", y))
  
  # Opening precipitation file
  nc <- nc_open(fname)
  prec <- ncvar_get(nc, "total_precipitation")
  
  # Selecting year length
  len <- 365
  if(y %% 4 == 0) len <- 366
  
  # Checking length
  if(len != dim(prec)[3]) stop(paste0("No correct amount of days for", y))
  rm(prec)
  rm(nc)
  gc()
}