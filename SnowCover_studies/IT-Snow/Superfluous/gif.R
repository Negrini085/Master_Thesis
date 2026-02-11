# The main goal of this script is to enable the user to plot SWE single-day snapshots
# To do so, ncdf4 and ggplot2 packages will be used
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

year <- 2011
months <- c("09","10","11","12","01","02","03","04","05","06","07","08")

# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")


maxSWE <- 0
# Opening files in order to find maximum swe value to fix colorbar during gif
for(i in 1:length(months)){
  if(i<=4){
    fname = paste0("y", toString(year), "/ITSNOW_SWE_", toString(year-1), months[i], ".nc")
  }
  else{
    fname = paste0("y", toString(year), "/ITSNOW_SWE_", toString(year), months[i], ".nc")
  }
  
  # Opening netCDF file and finding maximum value
  nc <- nc_open(fname)
  swe <- ncvar_get(nc, names(nc$var)[2])
  nc_close(nc)
  
  appo <- max(swe, na.rm = TRUE)
  if(appo > maxSWE){ maxSWE <- appo }
}


# Printing images
for(i in 1:length(months)){
  if(i<=4){
    fname = paste0("y", toString(year), "/ITSNOW_SWE_", toString(year-1), months[i], ".nc")
    title = paste0("SWE ", months[i],"-", year-1)
    yPr = year-1
  }
  else{
    fname = paste0("y", toString(year), "/ITSNOW_SWE_", toString(year), months[i], ".nc")
    title = paste0("SWE ", months[i],"-", year)
    yPR = year
  }

  # Opening netCDF file and reading its content
  nc <- nc_open(fname)
  lat <- ncvar_get(nc, names(nc$dim)[1])
  lon <- ncvar_get(nc, names(nc$dim)[2])
  time <- ncvar_get(nc, names(nc$var)[1])

  for(t in 1:length(time)){
    # Reading SWE and creating grid for plotting phase
    swe <- ncvar_get(nc, names(nc$var)[2], start = c(1, 1, t), count = c(-1, -1, 1))
    grid <- expand.grid(lon = lon, lat = lat)
    grid$swe <- as.vector(swe)

    # Plotting map
    p <- ggplot() +
      geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
      coord_sf(xlim = c(6, 19), ylim = c(37, 46.8)) +
      geom_raster(data = grid, aes(x = lon, y = lat, fill = swe)) +
      scale_fill_viridis_c(option = "C", limits = c(0, maxSWE), na.value = "transparent", oob = scales::squish) +
      labs(title = title, x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
      theme_minimal()

    # Creating correct file name in order to use ggsave
    if(i<= 4){
      fileout = paste0("Images/Gif/SWE_map_", sprintf("%02d", t), "-", months[i], "-", toString(year-1), ".png")
    }
    else{
      fileout = paste0("Images/Gif/SWE_map_", sprintf("%02d", t), "-", months[i], "-", toString(year), ".png")
    }
    ggsave(fileout, plot = p, width = 8, height = 6, dpi = 300)
    print(paste0("Salvata mappa ", sprintf("%02d", t), "-", months[i], "-", yPr))
  }
  nc_close(nc)

}
