# The main goal of this script is to make a comparison plot between MODIS snow 
# coverage and preliminary model SWE fields only on italian territory. To do so, 
# I will use Italian DEM to clip only onto italian territory.
rm(list = ls())
gc()

library(sf)
library(scico)
library(terra)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(patchwork)
library(ggnewscale)
library(rnaturalearth)

SWE_FILE   <- "Dataset/GRID_2017-04-25"
italian_dem <- "Superfluous/bacini_final.tif"
MODIS_FILE <- "../MODIS/Dataset/daily/2017/day_115.tif"
DEM_FILE   <- "../../Degree_Day_factor/DEM/DEM_stations_200.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Preliminary_model")


# Limits to make the plot better
XLIM <- c(6.6, 8.8)
YLIM <- c(45.2, 46.5)
SUN_ANGLE     <- 45
SUN_DIRECTION <- 315
SWE_MIN <- 0
SWE_MAX <- 100



# Importing swe datas
df  <- read.table(SWE_FILE, header = FALSE)
dem_italy <- rast(italian_dem)
lon <- as.numeric(df$V1)
lat <- as.numeric(df$V2)
swe <- as.numeric(df$V3)
rm(df); gc()

lat_max <- as.numeric(ext(dem_italy)[4])
mask <- lat > lat_max

lat[mask] <- NA
lon[mask] <- NA
swe[mask] <- NA

lat <- na.omit(lat)
lon <- na.omit(lon)
swe <- na.omit(swe)

lon_max <- as.numeric(ext(dem_italy)[2])
mask <- lon > lon_max

lat[mask] <- NA
lon[mask] <- NA
swe[mask] <- NA

lat <- na.omit(lat)
lon <- na.omit(lon)
swe <- na.omit(swe)

lat_min <- as.numeric(ext(dem_italy)[3])
mask <- sum(lat < lat_min)

lat[mask] <- NA
lon[mask] <- NA
swe[mask] <- NA

lat <- na.omit(lat)
lon <- na.omit(lon)
swe <- na.omit(swe)


points <- cbind(lon, lat)
inds   <- cellFromXY(dem_italy, points)

sum(is.na(inds))
sum(!is.na(inds))

appo   <- dem_italy[inds]
swe[is.na(appo)] <- NA

swe[swe <= 0] <- NA
grid_swe <- data.frame(lon = lon, lat = lat, swe = swe)
rm(lon, lat, swe); gc()


# Importing MODIS datas
bbox <- ext(XLIM[1]-0.1, XLIM[2]+0.1, YLIM[1]-0.1, YLIM[2]+0.1)
modis <- rast(MODIS_FILE)
modis <- crop(modis, bbox)
appo <- rast(italian_dem)
appo <- crop(appo, bbox)
compareGeom(modis, appo)
if (!is.lonlat(modis)) { modis <- project(modis, "EPSG:4326")}

modis[modis != 1] <- NA
modis <- ifel(!is.na(appo), modis, NA)
modis <- as.factor(modis)

dem <- rast(DEM_FILE)
dem  <- crop(dem, bbox)
if (!is.lonlat(dem)) { dem <- project(dem, "EPSG:4326")}


# Plotting topography
dem_smooth <- focal(dem, w = matrix(1, 7, 7), fun = mean, na.policy = "omit")
slope     <- terrain(dem_smooth, v = "slope",  unit = "radians")
aspect    <- terrain(dem_smooth, v = "aspect", unit = "radians")
hillshade <- shade(slope, aspect, angle = SUN_ANGLE, direction = SUN_DIRECTION)
hillshade <- (hillshade - minmax(hillshade)[1])/(minmax(hillshade)[2] - minmax(hillshade)[1])


# Plotting procedure
countries <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")
plot_modis <- ggplot() +
  geom_spatraster(data = hillshade, maxcell = Inf) +
  scale_fill_gradient(low = "grey50", high = "grey97", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = modis, maxcell = Inf) +
  scale_fill_manual(
    values   = c("1" = "#5B9BD5"), na.value = NA, na.translate = FALSE,
    name     = "MODIS", labels   = "Snow cover"
  ) +
  geom_sf(data = countries, fill = NA, color = "black", linewidth = 0.4, inherit.aes = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25, style = "bar", unit_category = "metric") +
  coord_sf(xlim = XLIM, ylim = YLIM, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "right",
    legend.key.height = unit(1, "cm"),
    panel.grid        = element_line(color = "grey80", linewidth = 0.2),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(), 
    plot.margin = margin(t = 15, b = 15)
  )

plot_swe <- ggplot() +
  geom_spatraster(data = hillshade, maxcell = Inf) +
  scale_fill_gradient(low = "grey50", high = "grey97", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_tile(data = grid_swe, aes(x = lon, y = lat, fill = swe), na.rm = TRUE) +
  scale_fill_scico(
    palette  = "roma", direction = 1, limits = c(SWE_MIN, SWE_MAX), oob = scales::squish,
    name     = "SWE [mm w.e.]", na.value = NA
  ) +
  geom_sf(data = countries, fill = NA, color = "black", linewidth = 0.4, inherit.aes = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25, style = "bar", unit_category = "metric") +
  coord_sf(xlim = XLIM, ylim = YLIM, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "right",
    legend.key.height = unit(2, "cm"),
    panel.grid        = element_line(color = "grey80", linewidth = 0.2), 
    plot.margin = margin(t = 15)
  )

final_plot <- plot_modis / plot_swe +
  plot_annotation(
    tag_levels = 'a', tag_prefix = '(', tag_suffix = ')',
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  ) &
  theme(plot.tag = element_text(margin = margin(r = 10))) 

ggsave(
  filename = "comparison_plot.png",
  plot     = final_plot,
  width    = 12,
  height   = 14,
  dpi      = 300,
  bg       = "white"
)
message("Done! Plot saved as comparison_plot.png")