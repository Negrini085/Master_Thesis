# The purpose of this script is to check if the two methods I used in order to 
# compute the monthly map for March 2011 give similar result or not. To do so, 
# we will subtract one matrix from the another, hoping to get almost all zero values.

library(ncdf4)
library(ggplot2)
library(matrixStats)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# Loading swe monthly maps that I will compare
e <- new.env()
load("March2011_map_t2.RData", envir = e)
first <- e$appo

e <- new.env()
load("March2011_map_t3.RData", envir = e)
second <- e$appo

# Opening a netCDF file in order to have a grid for plotting
nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nc_close(nc) 

# Creating grid and variable to plot
diff <- second - first
grid <- expand.grid(lon = lon, lat = lat)
grid$diff <- as.vector(diff)

ggplot(grid, aes(x = lon, y = lat, fill = diff)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C") +   # Continuous color palette
  coord_fixed() +
  labs(title = "Difference: try3 - try2", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +     # Titles
  theme_minimal()