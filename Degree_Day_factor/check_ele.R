# The main goal of this plot is to check if geo-data are indeed correct or not.
rm(list = ls())
gc()

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



# Reading coord.dat file, which is a file containing station names, coordinates 
# and altitude. We will then load the DEM model covering the whole alpine arc.
appo <- as.matrix(read.table("Dataset/coord.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:4]), ncol = 3)
station_names <- appo[, 1]
rm(appo)
gc()

dem <- rast("DEM/DEM_stations.tif")



# Actual procedure. We will check every station elevation on a 30 meter DEM. The
# output will be a file having 8 columns, the first being station name, the second 
# and third one containg station coordinates. On the fourth one altitude will be 
# listed, as the following 3 columns will then store closest DEM point infos.
dem_ele <- numeric(length = nrow(coord_ele))
dem_lat <- numeric(length = nrow(coord_ele))
dem_lon <- numeric(length = nrow(coord_ele))
for(station in 1:nrow(coord_ele)){
  # Selecting station to be analyzed
  point <- coord_ele[station, 1:2, drop = FALSE]
  
  # Raster point closest to station point
  ind <- cellFromXY(dem, point)
  coord_dem <- xyFromCell(dem, ind)
  dem_lon[station] <- coord_dem[1]
  dem_lat[station] <- coord_dem[2]

  # DEM grid point elevation
  dem_ele[station] <- as.numeric(dem[ind])
}

diff <- coord_ele[, 3] - dem_ele

# Checking for faulty elevations (here we will use a 15 meter tollerance window)
mark <- array("", dim = c(length(diff)))
mask <- diff > 15
mark[mask] <- "***"



# Building data-frame
results <- data.frame(
  name = station_names,
  lon = coord_ele[,1],
  lat = coord_ele[,2],
  ele = coord_ele[,3],
  lon_DEM = dem_lon, 
  lat_DEM = dem_lat,
  ele_DEM = dem_ele,
  diff = diff,
  mark = mark
)

write.table(results, file = "Datas/check_ele.dat", row.names = FALSE, col.names = TRUE)