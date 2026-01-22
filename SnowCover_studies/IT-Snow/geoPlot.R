# The main goal of this script is to plot SWE distribution, while also having 
# plotted Italian topography.

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/CODICINI/TESI_MAGISTRALE/SnowCover_studies/IT-Snow")

# Opening netCDF file and loading what we want to plot (this being the SWE snapshot
# of the first day of March). We will not get into details, because we already covered
# them in the previous couple of R files.
f_name <- "y2011/ITSNOW_SWE_201103.nc"
nc <- no_open(f_name)
lat_name = names(nc$dim)[1]
lon_name = names(nc$dim)[2]
swe_name = names(nc$var)[2]
lat <- ncvar_get(nc, lat_name)
lon <- ncvar_get(nc, lon_name)
swe <- ncvar_get(nc, swe_name, start = c(1, 1, 1), count = c(-1, -1, 1))


# Plotting snapshot without Italian boundaries. We covered this topic in the 
# previous file, so we will skip documenting lines.
grid <- expand.grid(lon = lon, lat = lat)
grid$swe <- as.vector(swe)
ggplot(grid, aes(x = lon, y = lat, fill = grid$swe)) + 
  geom_raster() + 
  scale_fill_viridis_c(option = "C") + 
  coor_fixed() + 
  labs(title = "SWE 1 March 2011", x = "Latitude", y = "Longitude", fill = "SWE (mm w.e.)") + 
  theme_minimal()

nc_close(nc)