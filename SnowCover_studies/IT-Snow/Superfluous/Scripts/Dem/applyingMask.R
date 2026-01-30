# The main goal of this script is to produce a DEM of Italy with every pixel 
# outside its contours beeing initialized to -100. By doing so, we can then do 
# our analysis without the need of using a mask
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow/DEM")
fileN <- "DEM_region.tif"
maskN <- "Mask.tif"

# Opening raster files
demR <- rast(fileN)
maskR <- rast(maskN)
compareGeom(demR, maskR, stopOnError = TRUE)

# Applying mask
appo <- demR
appo[maskR == 1] <- -100
dem <- demR
dem[appo != -100] <- -1000
writeRaster(dem, "DEM_Italy.tif", overwrite = TRUE)
