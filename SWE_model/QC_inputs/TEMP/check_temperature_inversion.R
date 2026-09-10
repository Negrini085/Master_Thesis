# The main goal of this script is to check whether some temperature inversions 
# can still be found or if it's all good
rm(list = ls()); gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")
years <- 1951:2023
block <- 46L

report <- list()

for (y in years) {
  fname <- paste0("../../Dataset/TEMP/", y, ".nc")
  if (!file.exists(fname)) stop("No temperature file for ", y)
  
  nc <- nc_open(fname)
  on.exit(nc_close(nc), add = TRUE)
  nt <- nc$dim$time$len
  
  for (t0 in seq(1L, nt, by = block)) {
    n <- min(block, nt - t0 + 1L)
    tmax <- ncvar_get(nc, "tmxd", start = c(1, 1, t0), count = c(-1, -1, n))
    tmin <- ncvar_get(nc, "tmnd", start = c(1, 1, t0), count = c(-1, -1, n))
    
    mask <- tmin > tmax
    if(any(mask, na.rm = TRUE)) stop(paste0("Temperature inversion for: ", y))
    rm(tmax, tmin, bad)
  }
  
  nc_close(nc)
  cat("Checked", y, "\n")
}