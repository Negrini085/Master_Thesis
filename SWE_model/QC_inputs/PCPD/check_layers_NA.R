# The main goal of this script is to check weather a single map can be made only
# of NAs
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Input/PCPD/")
years <- 1951:2023

for(y in years){
  fname <- paste0("../../../Input/PCPD/", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No precipitation file for ", y))
  
  # Opening precipitation maps
  nc <- nc_open(fname)
  prec <- ncvar_get(nc,"total_precipitation")
  
  for(i in 1:dim(prec)[3]){
    daily_map <- prec[, , i]
    if(all(is.na(daily_map))) stop(paste0("All datapoints are NAs during ", i, " day of ", y))
  }
  
  nc_close(nc)
  rm(prec)
  rm(nc)
  gc()
}