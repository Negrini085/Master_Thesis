# The main goal of this script is to double check whether the three temperature 
# variables are ordered as follows: tmin - tmean - tmax
rm(list = ls()); gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")
years <- 1951:2023

for (y in years) {
  fname <- paste0("../../../../Backup/TEMP/temperatures_", y, ".nc")
  if (!file.exists(fname)) stop("No temperature file for ", y)
  
  nc <- nc_open(fname)
  tmax <- ncvar_get(nc, "tmxd")
  tmin <- ncvar_get(nc, "tmnd")
  tmed <- ncvar_get(nc, "tmd")
  nc_close(nc)
  rm(nc); invisible(gc());
  
  stopifnot(identical(dim(tmin), dim(tmed)), identical(dim(tmed), dim(tmax)))
  
  mask <- (tmin > tmed | tmed > tmax) & !is.na(tmin) & !is.na(tmed) & !is.na(tmax)
  rm(tmax, tmin, tmed); invisible(gc())
  
  count <- sum(mask, na.rm = TRUE)
  cat("Year: ", y, "    Inversions: ",  count, "\n")
  cat("Checked", y, "\n")
  
  rm(mask); invisible(gc())
}