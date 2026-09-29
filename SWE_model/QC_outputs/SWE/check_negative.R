# The main goal of this script is to check whether some negative precipitation 
# values are present in this new dataset
rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_outputs/SWE/")
years <- 1945:2023

for(y in years){
  fname <- paste0("../../Results/SWE_", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No swe file for ", y))
  
  # Opening precipitation file
  nc <- nc_open(fname)
  prec <- ncvar_get(nc, "swe")
  
  mask <- prec < 0
  if(any(mask, na.rm = TRUE)) stop(paste0("Some negative swe during", y))
}