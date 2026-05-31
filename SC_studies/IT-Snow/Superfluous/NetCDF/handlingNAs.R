# We now have to inspect netCDF file content. Are you really sure that NA means 
# zero snow water equivalent, or NA is just used in order to define italian borders?

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

# We just want to filter pixels using their values. In particular, we will give 1
# to all the pixels that have SWE greater or equal to zero, whilst the other pixels 
# will be label as zeros
f_name = "y2011/ITSNOW_SWE_201103.nc"
nc <- nc_open(f_name)
lat_name <- names(nc$dim)[1]
lon_name <- names(nc$dim)[2]
val_name <- names(nc$var)[2]
lat = ncvar_get(nc, lat_name)
lon = ncvar_get(nc, lon_name)
swe = ncvar_get(nc, val_name, start = c(1, 1, 10), count = c(-1, -1, 1))
nc_close(nc)

# Applying the actual filter
grid <- expand.grid(lon = lon, lat = lat)
swe[swe >= 0] <- 1
swe[is.na(swe)] <- 0
grid$swe <- as.vector(swe)


ggplot(grid, aes(x = lon, y = lat, fill = swe)) +
  geom_raster() +                       
  scale_fill_viridis_c(option = "C") +
  coord_fixed() +   
  labs(title = "SWE 10 March 2011", x = "Latitude", y = "Longitude", fill = "SWE (mm w.e.)") +     # Titles
  theme_minimal()