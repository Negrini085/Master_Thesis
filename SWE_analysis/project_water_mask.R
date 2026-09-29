# The main goal of this script is to project the water map in order to correctly mask SWE fields
rm(list = ls())
gc()

library(terra)

fname_dem <- "DEM/DEM_compatible.tif"
fname_swe <- "Dataset/SWE/SWE_1945.nc"
fname_water <- "DEM/Copernicus_water.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Importing both raster
dem <- rast(fname_dem)
swe <- rast(fname_swe)
water <- rast(fname_water)
compareGeom(dem, water, stopOnError = FALSE)


# Selecting water pixels
mask <- water != 1
water[mask] <- 0
water[!mask] <- 1
water <- project(water, dem, method = "near")
n <- terra::global(!(water %in% c(0, 1)), "sum", na.rm = TRUE)[1, 1]
is.na(n) || n == 0


# Comparing geometries before saving water mask
compareGeom(water, dem)
compareGeom(water, swe)


# Saving to netCDF file
writeCDF(
  water, filename = "DEM/water_mask.nc", varname = "water", longname = "Copernicus water mask (1 = water, 0 = no water)",
  unit = "", prec = "byte", missval = -1, compression = 4, overwrite = TRUE
  )