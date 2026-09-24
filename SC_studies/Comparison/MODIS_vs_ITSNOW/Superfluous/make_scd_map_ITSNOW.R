# The main goal of this script is to compute mean SCD maps for ITSNOW reanalysis
rm(list = ls())
gc()

library(terra)

fname_dem <- "../../IT-Snow/DEM/DEM_Italy.tif"
fname_scd <- "../../IT-Snow/SCD/SCD-from-RHO/Datas/scd_mean_map.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/MODIS_vs_ITSNOW/")


# Importing dem and mean_scd to crop the Italian territory
dem <- rast(fname_dem)
mean_scd <- rast(fname_scd)
dem <- project(dem, mean_scd, method = "bilinear")


# Masking Italian territory
mask_ita <- is.na(dem)
mean_scd <- mask(mean_scd, mask_ita, maskvalues = 1)


# Saving raster to file
writeRaster(
  mean_scd, "Dataset/mean_scd_ITSNOW.tif", overwrite = TRUE, 
  datatype  = "FLT4S", gdal = c("COMPRESS=DEFLATE")
  )