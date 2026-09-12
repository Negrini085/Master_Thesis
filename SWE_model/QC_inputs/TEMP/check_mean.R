# The main goal of this script is to check whether the three temperature variables 
# are ordered as follows: tmin - tmean - tmax
rm(list = ls()); gc()

library(ncdf4)
 
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")
years <- 1951
block <- 46L

appo <- list()
for (y in years) {
  len <- 0
  count <- 0
  fname <- paste0("../../../../Backup/TEMP/temperatures_", y, ".nc")
  if (!file.exists(fname)) stop("No temperature file for ", y)
   
  nc <- nc_open(fname)
  nt <- nc$dim$time$len
  
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
   
  for (t0 in seq(1L, nt, by = block)) {
    n <- min(block, nt - t0 + 1L)
    tmax <- ncvar_get(nc, "tmxd", start = c(1, 1, t0), count = c(-1, -1, n))
    tmin <- ncvar_get(nc, "tmnd", start = c(1, 1, t0), count = c(-1, -1, n))
    tmed <- ncvar_get(nc, "tmd", start = c(1, 1, t0), count = c(-1, -1, n))
    
    
    # Checking raster size
    stopifnot(identical(dim(tmin), dim(tmed)), identical(dim(tmed), dim(tmax)))

         
    # Checking temperature inversion
    count <- 0
    mask <- (tmin > tmed | tmed > tmax) & !is.na(tmin) & !is.na(tmed) & !is.na(tmax)
    if(any(mask, na.rm = TRUE)){

      # Ready to select longitude, latitude and day value
      idx <- which(mask, arr.ind = TRUE)

      df <- data.frame(
        year = y,
        lon  = lon[idx[, 1]],
        lat  = lat[idx[, 2]],
        time = time[idx[, 3] + t0 - 1L],
        tmin = tmin[mask],
        tmed = tmed[mask],
        tmax = tmax[mask]
      )

      appo[[length(appo) + 1L]] <- df
      count <- sum(mask, na.rm = TRUE)
      len <- len + count
    }
    
    # Counting how many inversions we have
    rm(tmax, tmin, tmed)
    invisible(gc())
  }
  
  cat("Year: ", y, "    Inversions: ",  len, "\n")
  
  nc_close(nc)
  cat("Checked", y, "\n")
}

appo <- do.call(rbind, appo)
write.table(appo, "total_inversions.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)