# The main goal of this script is to assess DEM resolution and to crop it as needed. 
rm(list = ls())
gc()

library(ncdf4)
library(terra)

fname_DEM <- "DEM/original_DEM.tif"
fname_MAP <- "../../Input/PCPD/1951.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Study_area/")



# Importing netCDF which will act as a mask for cropping procedure
nc <- nc_open(fname_MAP)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
prec <- ncvar_get(nc, "total_precipitation")
tot_prec <- rowSums(prec, dims = 2, na.rm = FALSE)
nc_close(nc)
rm(prec)
gc()



# Creating total precipitation raster
xyz_prec <- expand.grid(lon = lon, lat = lat)
xyz_prec$prec <- as.vector(tot_prec)
prec_rast <- rast(xyz_prec, type = "xyz", crs = "EPSG:4326")
names(prec_rast) <- "total_precipitation"



# Importing DEM raster, projection and mask creation
dem <- rast(fname_DEM)
prec_on_dem <- project(prec_rast, dem, method = "near")
mask_prec <- ifel(is.na(prec_on_dem), NA, 1)



# Masking DEM and eliminating NAs
dem_masked <- mask(dem, mask_prec)
dem_cropped <- trim(dem_masked)



# Saving DEM (and evaluating resolution)
writeRaster(dem_cropped, "DEM/study_area_dem.tif", overwrite = TRUE)
print(res(dem_cropped))