# The main goal of this script is to produce a good-looking plot
library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nc_close(nc)


load("March2011_map.RData")


grid <- expand.grid(lon = lon, lat = lat)
grid$swe <- as.vector(appo)
grid$swe[grid$swe == 0] = NA

ggplot(grid, aes(x = lon, y = lat, fill = swe)) +
 geom_raster() +
 scale_fill_viridis_c(option = "C") +   # Continuous color palette
 coord_fixed() +
 labs(title = "SWE March 2011", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +     # Titles
 theme_minimal()

