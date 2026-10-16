# The main goal of this script is to enable the user to plot SWE single-day snapshots
# To do so, ncdf4 and ggplot2 packages will be used
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

fname <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_15.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")


# Opening SWE raster
nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
scd <- ncvar_get(nc, "scd")
nc_close(nc)

# Cycle over hydrological years
for(i in 1:dim(scd)[3]){

  # Plotting procedure
  appo_scd <- scd[, , i]
  grid <- expand.grid(lon = lon, lat = lat)
  grid$appo_scd <- as.vector(appo_scd)

  p <- ggplot() +
    geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
    coord_sf(xlim = c(6.2, 14.5), ylim = c(43, 47.1)) +
    geom_raster(data = grid, aes(x = lon, y = lat, fill = appo_scd)) +
    scale_fill_viridis_c(option = "C", na.value = "transparent", limits = c(0, 365), oob = scales::squish) +
    labs(title = paste("SCD -", (1950 + i)), x = "Longitude", y = "Latitude", fill = "SCD [days]") +
    theme_minimal()

  fileout = paste0("Images/SCD_maps/SCD_", (1950 + i), ".png")
  ggsave(fileout, plot = p, width = 8, height = 6, dpi = 300)
  print(paste0("Saved map ", (1950 + i)))
}