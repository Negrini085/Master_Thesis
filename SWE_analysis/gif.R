# The main goal of this script is to enable the user to plot SWE single-day snapshots
# To do so, ncdf4 and ggplot2 packages will be used
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

year <- 2001
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Selecting the geographical background, in order to really understand where the
# snow coverage actually is
europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")


# Opening SWE raster
nc <- nc_open(filename = paste0("Dataset/SWE_", year, ".nc"))
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
time <- ncvar_get(nc, "time")
swe <- ncvar_get(nc, "swe")
nc_close(nc)


# Setting zero as NA in order to focus only on the actual snow cover
mask <- swe == 0
swe[mask] <- NA

for(i in 1:length(time)){

  # Selecting target date
  dates <- as.Date("1950-01-01") + time[i]
  target_date <- as.Date(paste0(year, "-01-01")) + i - 1


  # Plotting procedure
  appo_swe <- swe[, , i]
  grid <- expand.grid(lon = lon, lat = lat)
  grid$appo_swe <- as.vector(appo_swe)

  p <- ggplot() +
    geom_sf(data = europe, fill = "grey90", color = "black", inherit.aes = FALSE) +
    coord_sf(xlim = c(6.2, 14.5), ylim = c(43, 47.1)) +
    geom_raster(data = grid, aes(x = lon, y = lat, fill = appo_swe)) +
    scale_fill_viridis_c(option = "C", limits = c(0, 1000), na.value = "transparent", oob = scales::squish) +
    labs(title = paste("SWE -", format(target_date, "%B %Y")), x = "Longitude", y = "Latitude", fill = "SWE (mm w.e.)") +
    theme_minimal()

  fileout = paste0("Images/Gif/SWE_map_", target_date, ".png")
  ggsave(fileout, plot = p, width = 8, height = 6, dpi = 300)
  print(paste0("Saved map ", target_date))
}