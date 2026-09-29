# The main goal of this script is to assess whether the same pixels are NA over 
# the whole study period.
rm(list = ls())
gc()

library(ncdf4)

years <- 1945:2023
fname_template <- "../../Results/SWE_1945.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_outputs/SWE/")


# Importing template file
nc <- nc_open(fname_template)
swe_mask <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, 1))
swe_mask <- is.na(swe_mask)
nc_close(nc)


for(y in years){
  
  # Selecting swe raster for a given year
  fname <- paste0("../../Results/SWE_", y, ".nc")
  nc <- nc_open(fname)
  swe <- ncvar_get(nc, "swe")
  swe <- is.na(swe)
  nc_close(nc)
  
  for(j in 1:dim(swe)[3]){
    if(any(swe[, , j] != swe_mask)) stop(paste0("Different NA mask in ", j, " of ", y, "!"))
  }

}