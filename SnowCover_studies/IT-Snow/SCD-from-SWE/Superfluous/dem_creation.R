# The main goal of this script is to create an Italian DEM which only has values 
# in the Italian territory. Outside are only NAs.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/DEM")

# Importing italian mask and digitalized elevation model
mask_ita <- rast("Mask.tif")
dem_region <- rast("DEM_region.tif")

# Before continuing, we first have to check whether mask and dem are comparable or not
if(compareGeom(mask_ita, dem_region)){
  mask_ita <- !is.na(mask_ita)
  dem_italy <- mask(dem_region, mask_ita, maskvalue = FALSE)
  writeRaster(dem_italy, "dem_italy.tif", overwrite = TRUE)
}
