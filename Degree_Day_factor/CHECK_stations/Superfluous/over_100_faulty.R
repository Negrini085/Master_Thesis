# The main goal of this script is to filter stations to find those whose elevation
# exceeds the 100 meters threshold with respect to a 30 meter resolution DEM. I 
# need to do so because I feel like maybe theris position can be obtained in a
# different way in order to keep more stations in our dataset.
rm(list = ls())
gc

library(terra)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



# Reading coord.dat file, which is a file containing station names, coordinates 
# and altitude. We will then load the DEM model covering the whole alpine arc.
appo <- as.matrix(read.table("Dataset/coord.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:4]), ncol = 3)
station_names <- appo[, 1]
rm(appo)
gc()

diff_lim <- 100
dem <- rast("DEM/DEM_stations_30.tif")



# Actual procedure. We will check every station elevation on a 30 meter DEM. The
# output will be a file having 5 columns, the first being station name, the second 
# and third one containg station coordinates. On the fourth one altitude will be 
# listed, as the following one will have DEM elevation
dem_ele <- numeric(length = nrow(coord_ele))
for(station in 1:nrow(coord_ele)){
  # Selecting station to be analyzed
  point <- coord_ele[station, 1:2, drop = FALSE]
  
  # Raster point closest to station point
  ind <- cellFromXY(dem, point)
  coord_dem <- xyFromCell(dem, ind)
  
  # DEM grid point elevation
  dem_ele[station] <- as.numeric(dem[ind])
}

diff <- abs(coord_ele[, 3] - dem_ele)



# Masking to get only highly biased stations
mask <- diff > diff_lim



# Building data-frame
results <- data.frame(
  name = station_names[mask],
  lon = coord_ele[,1][mask],
  lat = coord_ele[,2][mask],
  ele = coord_ele[,3][mask],
  ele_DEM = dem_ele[mask]
)

write.table(results, file = "Datas/over_100_faulty.dat", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")