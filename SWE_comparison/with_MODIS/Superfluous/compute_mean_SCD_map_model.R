# The main goal of this script is to compute mean SCD maps for ITSNOW reanalysis
rm(list = ls())
gc()

library(terra)

fname_mask <- "../../SC_studies/MODIS/Dataset/annual_maps/LOS/los_2023.tif"
fname_scd <- "../../SWE_analysis/Results/SCD/scd_maps_hydro_1952_to_2023_lim_15.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_MODIS/")


# Importing dem and mean_scd to crop the Italian territory
scd_maps <- rast(fname_scd)
mean_scd <- mean(scd_maps[[61:72]])

appo <- rast(fname_mask)
appo <- project(appo, mean_scd, method = "near")
overlap_mask <- !is.na(appo) & !is.na(mean_scd)
mean_scd[!overlap_mask] <- NA


# Saving raster to file
writeRaster(
  mean_scd, "Results/mean_scd_model.tif", overwrite = TRUE, 
  datatype  = "FLT4S", gdal = c("COMPRESS=DEFLATE")
  )