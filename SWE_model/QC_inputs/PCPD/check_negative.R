# The main goal of this script is to check whether some negative precipitation 
# values are present in this new dataset
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
  
  mask <- prec < 0
  if(any(mask, na.rm = TRUE)) stop(paste0("Some negative precipitation during", y))
}