  # The main goal of this script is to plot SWE distribution, while also having
  # plotted Italian topography.
  rm(list = ls())
  gc()
  
  library(sf)
  library(ncdf4)
  library(ggplot2)
  library(rnaturalearth)
  
  setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
  
  # Opening netCDF file and loading what we want to plot (this being the SWE snapshot
  # of the first day of March). We will not get into details, because we already covered
  # them in the previous couple of R files.
  f_name <- "y2013/ITSNOW_SWE_201308.nc"
  nc <- nc_open(f_name)
  lat_name = "Latitude"
  lon_name = "Longitude"
  swe_name = "SWE"
  lat <- ncvar_get(nc, lat_name)
  lon <- ncvar_get(nc, lon_name)
  swe <- ncvar_get(nc, swe_name, start = c(1, 1, 10), count = c(-1, -1, 1))
  # swe[swe == 0] <- NA
  
  
  # Selecting the geographical background, in order to really understand where the
  # snow coverage actually is
  europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
  
  # Plotting snapshot without Italian boundaries. We covered this topic in the
  # previous file, so we will skip documenting lines.
  grid <- expand.grid(lon = lon, lat = lat)
  grid$swe <- as.vector(swe)
  
  ggplot() +
    geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
    coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
    geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
    scale_fill_viridis_c(option = "C", na.value = "transparent") +
    labs(title = "SWE 10 August 2013", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
    theme_minimal()
  
  nc_close(nc)