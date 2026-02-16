# The main goal of this plot is to check if geo-data are indeed correct or not.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



# Reading coord.dat file, which is a file containing station names, coordinates 
# and altitude. We will then load the DEM model covering the whole alpine arc.
appo <- as.matrix(read.table("Dataset/coord.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:4]), ncol = 3)
rm(appo)
gc()

dem <- rast("DEM/DEM_stations.tif")

print(coord_ele[1, 1:2])
print(cellFromXY(dem, coord_ele[1, 1:2, drop = FALSE]))
print(xyFromCell(dem, cellFromXY(dem, coord_ele[1, 1:2, drop = FALSE])))

print(coord_ele[1, 3])
print(dem[cellFromXY(dem, coord_ele[1, 1:2, drop = FALSE])]$DEM_stations)