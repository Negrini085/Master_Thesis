# The main goal of this script is to check if something is wrong on some dates.
rm(list = ls())
gc()

library(ncdf4)

years <- seq(1945, 2023, 1)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")

for(y in years){
  fname <- paste0("../../Dataset/TEMP/", y, ".nc")
  if (!file.exists(fname)) stop("No temperature file for ", y)
  
  nc <- nc_open(fname)
  tmax <- ncvar_get(nc, "tmxd")
  tmin <- ncvar_get(nc, "tmnd")
  tmed <- ncvar_get(nc, "tmd")
  nc_close(nc)
  stopifnot(identical(dim(tmin), dim(tmed)), identical(dim(tmed), dim(tmax)))
  
  for(k in 1:dim(tmax)[3]){
    mask <- tmin[, , k] == tmed[, , k] & tmed[, , k] == tmax[, , k]
    if(all(mask, na.rm = TRUE)){
      cat("All temperature fields during", k, " of ", y, "are equal!", "\n")
      next
      }
    
    
    mask <- tmin[, , k] == tmax[, , k]
    if(all(mask, na.rm = TRUE)) cat("Minimum and maximum temperature fields during", k, " of ", y, "are equal!", "\n")
  }
  

  
  rm(tmin, tmed, tmax, mask); gc();
}