# The main goal of this script is to check if geo-data are indeed correct or not.
rm(list = ls())
gc()

library(terra)
library(ggplot2)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")



# Reading coord.dat file, which is a file containing station names, coordinates 
# and altitude. We will then load the DEM model covering the whole alpine arc.
appo <- as.matrix(read.table("Dataset/coord.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:4]), ncol = 3)
dem_ele <- numeric(length = nrow(coord_ele))
rm(appo)
gc()



# Cycle over stations to find dem elevation in order to compute the difference 
# between the declared elevation and the real one
dem <- rast("DEM/DEM_stations_200.tif")
for(station in 1:nrow(coord_ele)){
  
  # Raster point closest to station point
  point <- coord_ele[station, 1:2, drop = FALSE]
  ind <- cellFromXY(dem, point)
  coord_dem <- xyFromCell(dem, ind)
  
  # DEM grid point elevation
  dem_ele[station] <- as.numeric(dem[ind])
}

diff <- abs(coord_ele[, 3] - dem_ele)



# Evaluating number of faulty stations as a function of elevation threshold
diff_lim <- seq(5, 200, 5)
num_faulty <- numeric(0)
for(lim in diff_lim){
  
  # Mask to find faulty stations
  mask <- diff > lim
  appo <- length(diff[mask])
  num_faulty <- c(num_faulty, appo)
}



# Plotting procedure
df <- data.frame(
  limit = diff_lim, 
  number = num_faulty
)

ggplot(df, aes(x = limit, y = number)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "firebrick", size = 3) +
  geom_text(aes(label = number), vjust = -1.5, size = 3.5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Stations to be discarded as a function of difference threshold",
    subtitle = "DEM resolution 200 meters",
    x = "Difference threshold [m]",
    y = "Number of discarded"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )