# The main goal of this script is to plot a swe map of a given day within the 
# dataset range.
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(scales)
library(ggplot2)

fname <- "Dataset/SWE_1962_2023.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")


# Getting variables to plot
nc <- nc_open(fname)
lat <- ncvar_get(nc, "N")
lon <- ncvar_get(nc, "E")
map <- ncvar_get(nc, "SWECLQMD", start = c(1, 1, 168), count = c(-1, -1, 1))
nc_close(nc)


# Plotting procedure
grid <- expand.grid(E = lon, N = lat)
grid$swe <- as.vector(map)
grid <- grid[!is.nan(grid$swe), ]

ggplot(grid, aes(x = E, y = N, fill = swe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis", direction = 1, name = "SWE [m w.e]") +
  coord_equal() +
  scale_x_continuous(labels = ~ paste0(.x / 1000, " km")) +
  scale_y_continuous(labels = ~ paste0(.x / 1000, " km")) +
  labs(
    title = "SWE map: 15/02/1962",
    x = "E [m, LV95]",
    y = "N [m, LV95]"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))