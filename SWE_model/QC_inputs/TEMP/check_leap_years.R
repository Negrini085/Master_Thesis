# The main goal of this script is to check what is wrong with leap years.
rm(list = ls())
gc()

library(ncdf4)

years <- seq(1952, 2020, 4)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")

for(y in years){
  fname <- paste0("../../../../Backup/TEMP/temperatures_", y, ".nc")
  if (!file.exists(fname)) stop("No temperature file for ", y)
  
  nc <- nc_open(fname)
  tmax <- ncvar_get(nc, "tmxd")
  tmin <- ncvar_get(nc, "tmnd")
  tmed <- ncvar_get(nc, "tmd")
  nc_close(nc)
  
  stopifnot(identical(dim(tmin), dim(tmed)), identical(dim(tmed), dim(tmax)))
  mask <- tmin == tmed & tmed == tmax
  
  if(all(mask, na.rm = TRUE)) cat("All temperature fields during", y, "are equal!", "\n")
  rm(tmin, tmed, tmax, mask); gc();
}