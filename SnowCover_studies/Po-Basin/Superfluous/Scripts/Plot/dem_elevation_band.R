# The main goal of this script is to check whether DEM was created correctly and
# if it could be a useful tool for elevation-based analysis
rm(list = ls())
gc()

library(sf)
library(terra)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

# Loading raster and comparing georef
swe <- rast("Dataset/1992/SWE_1991-11-10.tif")
dem <- rast("Dataset/Po_DEM.tif")
crs(dem)

if(compareGeom(swe, dem, stopOnError = FALSE)){
  print("Mappe georeferienzate in modo confrontabile fra loro!")
}


# Creating a mask. Thanks to maskvalues outside of the mask every value is set to NA
mask <- dem > 500 & dem <= 1000 & !is.na(swe)
dem_masked <- mask(dem, mask, maskvalues = FALSE)


# Plotting masked dem
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
europe  <- vect(europe)
europe <- project(europe, crs(dem_masked))

plot(dem_masked, axes = FALSE, main = "Po Basin DEM: 500 to 1000 meters")
lines(europe, col = "black", lwd = 1)

e <- ext(dem_masked)
rect(e$xmin, e$ymin, e$xmax, e$ymax, border = "black", lwd = 1)