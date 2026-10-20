# The main goal of this script is to make trend estimates for every day of the season on an elevation_basis
rm(list = ls())
gc()

library(terra)

years <- 1951:2023
fname_dem <- "DEM/DEM_compatible.tif"
fname_template <- "Dataset/SWE/SWE_1945.nc"
fname_glacier_mask <- "Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")
is_leap <- function(y) (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0


# Importing DEM, quality checking grid compatibility, selecting elevation bands and classifying
dem <- rast(fname_dem)
template <- rast(fname_template)[[1]]
compareGeom(dem, template, stopOnError = TRUE)

breaks  <- c(-Inf, seq(50, 3000, 50))
n_bands <- length(breaks) - 1
band_labels <- c("<=50", paste0(breaks[2:n_bands], "-", breaks[3:(n_bands+1)]))

zones <- classify(dem, cbind(breaks[-(n_bands + 1)], breaks[-1], 1:n_bands), right = TRUE, others = NA)
names(zones) <- "band"



# Creating glacier mask
glacier_mask <- rast(fname_glacier_mask)
glacier_mask <- glacier_mask > 36



# Cycle over years (to fill the matrix to make trend assessment on)
swe_band_mean <- array(NA_real_, dim = c(365, n_bands, length(years)), dimnames = list(day = 1:365, band = band_labels, year = years))
for (k in seq_along(years)) {
  
  # Importing swe maps and masking for glacier presence
  y <- years[k]
  swe <- rast(paste0("Dataset/SWE/SWE_", y,".nc"))
  swe <- mask(swe, glacier_mask, maskvalues = TRUE)
  
  
  # Checking number of layers in a given SWE file
  if (nlyr(swe) != 365 + is_leap(y)) stop(y, " file has ", nlyr(swe), " days instead of ", 365 + is_leap(y))
  if (is_leap(y)) swe <- swe[[-60]]
  
  # Computing mean swe value per elevation band
  zm <- zonal(swe, zones, fun = "mean", na.rm = TRUE)
  swe_band_mean[, as.integer(zm$band), k] <- t(as.matrix(zm[, -1]))
  
  cat("Taken into account year ", y, "!\n")
}

# Saving results (converting in hydrological year format)
seasons <- years[-1]
swe_season <- array(NA_real_, dim = c(365, n_bands, length(seasons)), dimnames = list(day_hydro = 1:365, band = band_labels, season = seasons))
for (k in seq_along(seasons)) {
  swe_season[1:92,   , k] <- swe_band_mean[274:365, , k]
  swe_season[93:365, , k] <- swe_band_mean[1:273,   , k + 1]
}
saveRDS(swe_season, "Results/swe_band_mean_hydrological.rds")