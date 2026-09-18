# The main goal of this script is create mean SWE maps over November - June period for ITSNOW
rm(list = ls())
gc()

library(terra)

fname_mask <- "Dataset/DEM_Italy.tif"
fname <- "../../IT-Snow/Datas/swe_seasonal_maps.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")

dem <- rast(fname_mask)
season_maps <- rast(fname)
mean_map <- mean(season_maps)

compareGeom(dem, mean_map, stopOnError = FALSE)
dem <- project(dem, mean_map, method = "bilinear")

mask <- is.na(dem)
mean_map[mask] <- NA

writeRaster(
  mean_map, "Dataset/mean_SWE_ITSNOW.tif", overwrite = TRUE,
  datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "TILED=YES")
)