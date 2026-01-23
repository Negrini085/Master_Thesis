# The purpose of this script is to verify the correctness of the netCDF file creation procedure.
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
fname <- "exampleNetCDF.nc"

nc <- nc_open(fname)
time <- ncvar_get(nc, names(nc$var)[1])
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
swe <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, 3), count = c(-1, -1, 1))
nc_close(nc)

# Plotting the resulting SWE map
grid <- expand.grid(lon = lon, lat = lat)
grid$swe <- as.vector(swe)
ggplot(grid, aes(x = lon, y = lat, fill = grid$swe)) + 
  geom_raster() + 
  scale_fill_viridis_c(option = "C") + 
  coord_fixed() + 
  labs(title = "SWE 3 March 2011", x = "Latitude", y = "Longitude", fill = "SWE (mm w.e.)") + 
  theme_minimal()