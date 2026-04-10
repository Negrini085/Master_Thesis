# The main goal of this script is to plot AWS DDFs
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

fname <- "Results/station_ddf.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Importing station ddf and infos
df <- read.table(fname, header = TRUE)
station_lon <- as.numeric(df$lon)
station_lat <- as.numeric(df$lat)
station_ele <- as.numeric(df$ele)
station_ddf <- as.numeric(df$ddf)
station_names <- df$name


# Plotting procedure
df_plot <- data.frame(lon = station_lon, lat = station_lat, ddf = station_ddf)
df_plot <- na.omit(df_plot)

europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(data = df_plot, aes(x = lon, y = lat, color = ddf), 
             size = 1.2, alpha = 0.8) +
  scale_color_viridis_c(option = "turbo", name = "DDF [mm/(°C · day)]") +
  coord_sf(xlim = c(9, 17.7), ylim = c(46, 49), expand = FALSE) +
  theme_minimal() +
  labs(title = "Spatial Distribution of DDF",
       x = "Longitude",
       y = "Latitude") +
  theme(panel.background = element_rect(fill = "aliceblue"))
