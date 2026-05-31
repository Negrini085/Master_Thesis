# The main goal of this script is to plot a visually nice map of the investigated 
# region. I would like to show the underlying topography, to underline how the biggest
# amount of snow water equivalent is located on the highest regions of the alps.
rm(list = ls())
gc()

library(sf)
library(scico)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggnewscale)
library(rnaturalearth)
library(patchwork)

SWE_FILE   <- "Dataset/GRID_2017-04-26"
DEM_FILE   <- "../../Degree_Day_factor/DEM/DEM_stations_200.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Preliminary_model")



# Limits to make the plot better
XLIM <- c(6.3, 14.0)
YLIM <- c(43.5, 47.5)

SUN_ANGLE     <- 45
SUN_DIRECTION <- 315

SWE_MIN <- 0
SWE_MAX <- 100



# Importing swe dataset
df  <- read.table(SWE_FILE, header = FALSE)
lon <- df$V1
lat <- df$V2
swe <- df$V3
rm(df); gc()

swe[swe <= 0] <- NA
grid <- data.frame(lon = lon, lat = lat, swe = swe)
rm(lon, lat, swe); gc()



# Computing hillshade, in order to plot topography with shades (than normalizing 
# in order to have unitary values)
dem <- rast(DEM_FILE)
bbox <- ext(XLIM[1]-0.1, XLIM[2]+0.1, YLIM[1]-0.1, YLIM[2]+0.1)

dem  <- crop(dem, bbox)
if (!is.lonlat(dem)) {dem <- project(dem, "EPSG:4326")}

dem_smooth <- focal(dem, w = matrix(1, 7, 7), fun = mean, na.policy = "omit")
slope     <- terrain(dem_smooth, v = "slope",  unit = "radians")
aspect    <- terrain(dem_smooth, v = "aspect", unit = "radians")

hillshade <- shade(slope, aspect, angle = SUN_ANGLE, direction = SUN_DIRECTION)
hillshade <- (hillshade - minmax(hillshade)[1])/(minmax(hillshade)[2] - minmax(hillshade)[1])



# Plotting procedure
countries <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
final_plot <- ggplot() +
  geom_spatraster(data = hillshade) +
  scale_fill_gradient(low = "grey50", high = "grey97", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = grid, aes(x = lon, y = lat, fill = swe), na.rm = TRUE) +
  scale_fill_scico(
    palette   = "roma", direction = 1, limits = c(SWE_MIN, SWE_MAX), oob = scales::squish,
    name      = "SWE (mm w.e.)", na.value  = NA
  ) +
  scale_alpha_continuous(range = c(0, 0.9), guide = "none", na.value = 0) +
  geom_sf(
    data        = countries, fill = NA, color = "black",
    linewidth   = 0.4, inherit.aes = FALSE
  ) +
  coord_sf(xlim = XLIM, ylim = YLIM, expand = FALSE) +
  labs(title = "Snow Water Equivalent", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position       = "right",
    legend.key.height     = unit(2, "cm"),
    panel.grid            = element_line(color = "grey80", linewidth = 0.2),
    plot.title            = element_text(face = "bold")
  )



# Saving swe map
ggsave(
  filename = "swe_map.png",
  plot     = final_plot,
  width    = 12,
  height   = 8,
  dpi      = 300,
  bg       = "white"
)
message("Done! Plot saved as swe_map.png")