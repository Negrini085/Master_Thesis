# The main goal of this script is to convert modis annual files into geoTiff in 
# order to use them to check station data quality. 
rm(list = ls())
gc()

library(terra)
library(hdf5r)
library(R.matlab)

year <- 2025
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS/")


# Reading snow cover datas from annual file
f1 <- H5File$new(paste0("Dataset/mod_", year, ".mat"), mode = "r")
sc_matrix <- f1[["MOD_nstep"]]$read()
f1$close_all()


# Reading coordinates
misc <- readMat("Dataset/misc_data.mat")
lat <- misc$lat[, 1]
lon <- misc$lon[, 1]


# Creating template raster and getting ready to fill it
r_template <- rast("Dataset/annual_maps/LOS/los_2025.tif")
r_template[] <- NA
cell_idx <- cellFromXY(r_template, cbind(lon, lat))


# Creating daily raster maps
n_days <- ncol(sc_matrix)
for (d in seq_len(n_days)) {
  r_day <- r_template
  vals <- rep(NA_real_, ncell(r_day))
  vals[cell_idx] <- sc_matrix[, d]
  values(r_day) <- vals

  fname_out <- paste0("Dataset/daily/", year, "/day_", sprintf("%03d", d), ".tif")
  writeRaster(r_day, fname_out, overwrite = TRUE, datatype = "INT1U")

  if (d %% 10 == 0) print(paste0("Dealing with day ", d, "/", n_days, " of ", year))
}