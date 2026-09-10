# The main goal of this script is to check whether the three temperature variables 
# are ordered as follows: tmin - tmean - tmax
rm(list = ls()); gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")
years <- 1951:2023
block <- 46L

report <- list()

for (y in years[1]) {
  fname <- paste0("../../Dataset/TEMP/", y, ".nc")
  if (!file.exists(fname)) stop("No temperature file for ", y)
  
  nc <- nc_open(fname)
  nt <- nc$dim$time$len
  
  for (t0 in seq(1L, nt, by = block)) {
    n <- min(block, nt - t0 + 1L)
    tmax <- ncvar_get(nc, "tmxd", start = c(1, 1, t0), count = c(-1, -1, n))
    tmin <- ncvar_get(nc, "tmnd", start = c(1, 1, t0), count = c(-1, -1, n))
    tmed <- ncvar_get(nc, "tmd", start = c(1, 1, t0), count = c(-1, -1, n))
    
    
    # Checking temperature existence
    mask <- is.na(tmin) & is.na(tmax) & !is.na(tmed)
    if(any(mask, na.rm = TRUE)) stop(paste0("Min and Max existing, Mean non existing during ", y))
    
    mask <- is.na(tmin) & !is.na(tmax) & is.na(tmed)
    if(any(mask, na.rm = TRUE)) stop(paste0("Min and Mean existing, Max non existing during ", y))
    
    mask <- !is.na(tmin) & is.na(tmax) & is.na(tmed)
    if(any(mask, na.rm = TRUE)) stop(paste0("Max and Mean existing, Min non existing during ", y))
    
    
    # Checking temperature inversion
    mask <- tmin > tmed | tmed > tmax & !is.na(tmin)
    if(any(mask, na.rm = TRUE)){
      mask[is.na(mask)] <- FALSE
      print(tmin[mask][1:10])
      print(tmed[mask][1:10])
      print(tmax[mask][1:10])
      stop(paste0("Some problems with mean values during : ", y))
    }
    
    rm(tmax, tmin, tmed)
    invisible(gc())
  }
  
  nc_close(nc)
  cat("Checked", y, "\n")
}