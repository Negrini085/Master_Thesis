# The main goal of this script it to compare model outputs in order to make sure 
# that the one that  I am publishing in this thesis is correct even if some small 
# modifications at the code have been done
rm(list = ls())
gc()

library(ncdf4)

years <- 1945:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/")


for(y in years){
  fname_model <- paste0("Results/SWE_", y, ".nc")
  fname_thesis <- paste0("Results/Appo/SWE_", y, ".nc")
  
  nc <- nc_open(fname_model)
  swe_model <- ncvar_get(nc, "swe")
  nc_close(nc)
  
  nc <- nc_open(fname_thesis)
  swe_thesis <- ncvar_get(nc, "swe")
  nc_close(nc)
  
  mask <- swe_model != swe_thesis
  if(any(mask, na.rm = TRUE)) cat("No compatible SWE values during ", y, "\n")
  
  mask <- is.na(swe_model) != is.na(swe_thesis)
  if(any(mask, na.rm = TRUE)) cat("NA pixels do not coincide during ", y, "\n")
}