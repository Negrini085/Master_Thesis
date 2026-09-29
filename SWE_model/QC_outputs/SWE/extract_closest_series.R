# The main goal of this script is to extract the closest series to my station 
# points in order to make visual comparisons between rasterized and sequential 
# outputs, keeping in mind that I am comparing different grid-points.
rm(list = ls())
gc()

library(ncdf4)

years <- 1950:2023
fname_mask <- "../../Results/SWE_1950.nc"
fname_ana <- "../../../HS_series/Correct/STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_outputs/SWE/")


# Importing station meta-data
df_ana <- read.table(fname_ana, header = TRUE)
lon <- as.numeric(df_ana$lon)
lat <- as.numeric(df_ana$lat)
name <- df_ana$name



# Importing SWE mask
nc <- nc_open(fname_mask)
mask <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, 1))
lon_grid <- ncvar_get(nc, "lon")
lat_grid <- ncvar_get(nc, "lat")
mask <- is.na(mask)
nc_close(nc)



# Checking which stations fall on a valid pixel
keep <- rep(FALSE, length(name))
ix <- rep(NA, length(name))
iy <- rep(NA, length(name))

for (i in 1:length(name)) {
  
  if (lon[i] < min(lon_grid) | lon[i] > max(lon_grid) |
      lat[i] < min(lat_grid) | lat[i] > max(lat_grid)) next
  
  # Closest grid-point & keeping only non NA
  ix[i] <- which.min(abs(lon_grid - lon[i]))
  iy[i] <- which.min(abs(lat_grid - lat[i]))
  if (!mask[ix[i], iy[i]]) keep[i] <- TRUE
}

df_ana <- df_ana[keep, ]
ix <- ix[keep]
iy <- iy[keep]










# Series extraction
n_st <- nrow(df_ana)
df_out <- NULL

for(y in years){
  fname <- paste0("../../Results/SWE_", y, ".nc")
  nc <- nc_open(fname)
  swe <- ncvar_get(nc, "swe")
  time <- ncvar_get(nc, "time")
  t_units <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  # Dates (assuming units like "days since YYYY-MM-DD ...")
  origin <- as.Date(substr(sub(".*since ", "", t_units), 1, 10))
  dates <- origin + time
  
  # One column per station
  mat <- matrix(NA, nrow = length(dates), ncol = n_st)
  for (i in 1:n_st) {
    mat[, i] <- swe[ix[i], iy[i], ]
  }
  
  df_out <- rbind(df_out, data.frame(date = dates, mat))
  cat("Series extracted for: ", y, "\n")
}



# Saving SWE series to file
dir_out <- "Results/SWE_series/"
dir.create(dir_out, showWarnings = FALSE)

for (i in 1:n_st) {
  df_st <- data.frame(date = df_out$date, swe = df_out[, i + 1])
  fname_out <- paste0(dir_out, df_ana$name[i])
  write.table(df_st, file = fname_out, row.names = FALSE, 
              col.names = TRUE, quote = FALSE, sep = "\t")
}



# Saving swe points to file
df_pix <- data.frame(
  name = df_ana$name, lon = df_ana$lon, lat = df_ana$lat,
  lon_grid = lon_grid[ix], lat_grid = lat_grid[iy])
write.table(df_pix, file = paste0(dir_out, "ANAGRAFICA_grid"), 
            row.names = FALSE, quote = FALSE, sep = "\t")