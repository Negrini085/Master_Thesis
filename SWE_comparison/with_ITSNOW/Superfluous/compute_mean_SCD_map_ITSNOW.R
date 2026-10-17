# The main goal of this script is to compute the SCD map for ITSNOW on the model grid and overlap region.
rm(list = ls())
gc()

library(terra)

fname_dem <- "../../SC_studies/IT-Snow/DEM/DEM_Italy.tif"
fname_itsnow <- "../../SC_studies/IT-Snow/SCD/SCD-from-RHO/Datas/scd_annual_maps.nc"
template_name <- "../../SWE_analysis/Results/SCD/scd_maps_hydro_1952_to_2023_lim_15.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_ITSNOW/")


italy_mask <- rast(fname_dem)
scd_mean <- mean(rast(fname_itsnow)[[1:13]])
template_map <- mean(rast(template_name))

italy_mask <- project(italy_mask, scd_mean, method = "near")
scd_mean <- mask(scd_mean, italy_mask)
scd_mean <- project(scd_mean, template_map, method = "near")


overlap_mask <- !is.na(scd_mean) & !is.na(template_name)
scd_mean <- mask(scd_mean, overlap_mask, maskvalues = FALSE)

contour_mask <- !(scd_mean == 0 & template_map > 50) 
scd_mean <- mask(scd_mean, contour_mask, maskvalues = FALSE)

# Saving raster to file
writeRaster(
  scd_mean, "Results/mean_scd_ITSNOW.tif", overwrite = TRUE, 
  datatype  = "FLT4S", gdal = c("COMPRESS=DEFLATE")
)