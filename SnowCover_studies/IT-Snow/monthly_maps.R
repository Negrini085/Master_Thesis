# The main goal of this script is to enable the user to compute swe monthly maps.
# In particular we would like to have as output a netCDF file for every hydrological
# year containing monthly SWE maps
rm(list = ls())
gc()

library(ncdf4)
library(matrixStats)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

years <- 2011#:2025
months <- c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08")

nc <- nc_open("y2011/ITSNOW_SWE_201103.nc")
lat <- ncvar_get(nc, names(nc$dim)[1])
lon <- ncvar_get(nc, names(nc$dim)[2])
nlat <- length(lat)
nlon <- length(lon)
nc_close(nc)

stackSWE <- array(0, dim = c(nlon, nlat, 12))

for(y in years){
  for(i in 1:length(months)){
    if(i<=4){
      ypr = y-1
      fname = paste0("y", toString(y), "/ITSNOW_SWE_", toString(y-1), months[i], ".nc")
    }
    else{
      ypr = y
      fname = paste0("y", toString(y), "/ITSNOW_SWE_", toString(y), months[i], ".nc")
    }

    print(paste0("Dealing with ", months[i], "/", ypr," SWE maps"))

    nc <- nc_open(fname)
    time <- ncvar_get(nc, names(nc$var)[1])
    swe <- ncvar_get(nc, names(nc$var)[2])
    swe[is.na(swe)] <- 0

    monthMap <- rowMeans(swe, dims = 2)
    stackSWE[, , i] <- monthMap

    grid <- expand.grid(lat = lat, lon = lon)
    monthMap[monthMap == 0] <- NA
    grid$swe <- as.vector(monthMap)
    europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
    p <- ggplot() +
      geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
      coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
      geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
      scale_fill_viridis_c(option = "C", na.value = "transparent") +
      labs(title = "SWE 1 March 2011", x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
      theme_minimal()

    ggsave(paste0("maps", i, ".png"), plot = p, width = 8, height = 6, dpi = 300)

    nc_close(nc)
  }

  # Creating netCDF dimensions
  lat_dim <- ncdim_def("lat", "degrees_north", lat)
  lon_dim <- ncdim_def("lon", "degrees_east", lon)
  time_dim <- ncdim_def("time", "Months in hydro year -> Sept to Aug", 1:12, unlim = TRUE)

  # Creating netCDF variable
  swe_var <- ncvar_def(
    name = "SWE", units = "mm (w.e.)", dim = list(lon_dim, lat_dim, time_dim),
    missval = -9999, longname = "Snow Water Equivalent", prec = "float"
  )

  # Creating netCDF and filling SWE values
  nc_out <- nc_create(paste0("MonthlyMaps", y, ".nc"), vars = list(swe_var))
  ncvar_put(nc_out, swe_var, stackSWE)

  stackSWE <- array(0, dim = c(nlon, nlat, 12))
  nc_close(nc_out)
}
