# The main goal of this script is to plot the increase in seasonal swe map
rm(list = ls())
gc()

library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(scales)
library(geodata)
library(ggspatial)

fname_dem <- "../IT-Snow/DEM/DEM_region.tif"
fname_map_1990 <- "Datas/seasonal_map_1992_2001.tif"
fname_map_2020 <- "Datas/seasonal_map_2012_2021.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Po-Basin/")


# Importing both seasonal maps (and dem)
dem <- rast(fname_dem)
map_1990 <- rast(fname_map_1990)
map_2020 <- rast(fname_map_2020)

correct_ext <- project(ext(map_1990), from = crs(map_1990), to = crs(dem))
map_1990 <- project(map_1990, dem, method = "bilinear")
map_2020 <- project(map_2020, dem, method = "bilinear")
map_1990 <- crop(map_1990, correct_ext)
map_2020 <- crop(map_2020, correct_ext)

mask <- map_1990 == 0
map_1990[mask] <- NA

diff <- (map_2020 - map_1990)/map_1990 * 100
names(diff) <- "diff"

val_min <- -50
val_max <- 50

map_title        <- ""
map_legend_title  <- "IT-SNOW \n over \n Po District"
col_high <- "#0000FF" 
col_mid   <- "white"
col_low <- "#E00000"
map_na_color <- NA

size_title        <- 15
size_axis_title   <- 20
size_axis_text    <- 15
size_legend_title <- 20
size_legend_text  <- 15

border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
italy_border <- crop(world_borders, ext(diff))


p_map <- ggplot() +
  geom_spatraster(data = diff, aes(fill = diff)) +
  geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradient2(
    low      = col_low,
    mid      = col_mid,
    high     = col_high,
    midpoint = 0,
    limits   = c(val_min, val_max),
    breaks   = seq(val_min, val_max, by = 25),
    labels   = function(x) paste0(x, "%"),
    oob      = function(x, range) scales::squish(x, range, only.finite = FALSE),
    na.value = map_na_color,
    name     = map_legend_title
  ) +
  labs(title = NULL, x = "Longitude [°E]", y = "Latitude [°N]") +
  theme_minimal(base_size = size_axis_text) +
  theme(
    plot.title      = element_text(size = size_title, face = "bold", hjust = 0.5),
    axis.text       = element_text(size = size_axis_text),
    axis.title      = element_text(size = size_axis_title),
    legend.title    = element_text(size = size_legend_title),
    legend.text     = element_text(size = size_legend_text),
    legend.position = "right"
  ) +
  coord_sf(expand = FALSE)

print(p_map)