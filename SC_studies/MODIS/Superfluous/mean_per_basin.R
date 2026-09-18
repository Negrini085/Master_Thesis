# The main goal of this script is to compute mean SCD values in a given basin 
# across the investigated area
rm(list = ls())
gc()

library(terra)

basin_num <- 5
years <- 2001:2025
fname_dem <- "DEM/MODIS_dem.tif"
fname_basin <- "Dataset/bacini_final.tif"
fname_scd <- "Datas/mean_maps/mean_los.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS/")



# Importing both dataset and DEM model
dem <- rast(fname_dem)
basin <- rast(fname_basin)
scd_map  <- rast(fname_scd)

compareGeom(dem, scd_map)
compareGeom(dem, basin)



# Computing mean SCD across the Italian territory
mean_scd <- global(scd_map, "mean", na.rm = TRUE)
mean(mean_scd$mean)



# Computing mean SCD over a given region
mask <- basin == basin_num
appo <- scd_maps
appo[!mask] <- NA

mean_scd <- global(appo, "mean", na.rm = TRUE)
mean(mean_scd$mean)
sd(mean_scd$mean)



# Computing mean SCD over a given region and above a certain elevation
mask <- basin == basin_num & dem > 500
appo <- scd_maps
appo[!mask] <- NA

mean_scd <- global(appo, "mean", na.rm = TRUE)
mean(mean_scd$mean)
sd(mean_scd$mean)