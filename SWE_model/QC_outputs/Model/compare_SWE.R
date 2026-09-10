# The main goal of this script is to compare SWE series obtained through a sequential 
# and a raster model (which should give the same results). This is a fundamental step 
# in my thesis work, as we are making sure that what comes next is the output of the 
# exact pipeline we would like to use.
rm(list = ls())
gc()

library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Model/")

for(y in years[43]){
  
  # Importing SWE datas
  fname_seq <- paste0("Dataset/Sequential/SWE_", y, ".nc")
  nc <- nc_open(fname_seq)
  swe_seq <- ncvar_get(nc, "swe")
  nc_close(nc)
  
  fname_ras <- paste0("Dataset/Raster/SWE_", y, ".nc")
  nc <- nc_open(fname_ras)
  swe_ras <- ncvar_get(nc, "swe")
  nc_close(nc)
  
  if(dim(swe_ras)[1] != dim(swe_seq)[1]) stop(paste0("No compatible dims for ", y))
  if(dim(swe_ras)[2] != dim(swe_seq)[2]) stop(paste0("No compatible dims for ", y))

  mask <- swe_ras != swe_seq
  if(any(mask)){ stop(paste0("No compatible values during ", y)) }
}