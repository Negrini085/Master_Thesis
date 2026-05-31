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

SWE_FILE    <- "Dataset/GRID_2017-04-25"
italian_dem <- "Superfluous/bacini_final.tif"
MODIS_FILE  <- "../MODIS/Dataset/daily/2017/day_115.tif"
DEM_FILE    <- "../../Degree_Day_factor/DEM/DEM_stations_200.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Preliminary_model")

XLIM <- c(9, 11)
YLIM <- c(45.7, 46.7)
SUN_ANGLE     <- 45
SUN_DIRECTION <- 315
SWE_MIN       <- 0
SWE_MAX       <- 100
LIM_MAX       <- 110   # upper limit extension for the grey overflow band

# Palette with a HARD transition at 100: roma colormap up to 100, then uniform grey
n         <- 512
roma_cols <- scico(n, palette = "roma")
cols_all  <- c(roma_cols, roma_cols[n], "grey55", "grey55")

# Normalized values on [0,1] relative to LIM_MAX
# The cut point is duplicated to create a sharp step (no interpolation)
cut      <- SWE_MAX / LIM_MAX
vals_all <- c(
  seq(0, cut, length.out = n + 1),  # roma from 0 to 100
  cut + 1e-6,                        # grey starts just above 100
  1                                  # grey ends at LIM_MAX
)

# ------------------------------------------------------------------------------
# Load and clip SWE data to Italian extent
# ------------------------------------------------------------------------------
df        <- read.table(SWE_FILE, header = FALSE)
dem_italy <- rast(italian_dem)
lon <- as.numeric(df$V1)
lat <- as.numeric(df$V2)
swe <- as.numeric(df$V3)
rm(df); gc()

lat_max <- as.numeric(ext(dem_italy)[4])
mask    <- lat > lat_max
lat[mask] <- NA; lon[mask] <- NA; swe[mask] <- NA
lat <- na.omit(lat); lon <- na.omit(lon); swe <- na.omit(swe)

lon_max <- as.numeric(ext(dem_italy)[2])
mask    <- lon > lon_max
lat[mask] <- NA; lon[mask] <- NA; swe[mask] <- NA
lat <- na.omit(lat); lon <- na.omit(lon); swe <- na.omit(swe)

lat_min <- as.numeric(ext(dem_italy)[3])
mask    <- lat < lat_min
lat[mask] <- NA; lon[mask] <- NA; swe[mask] <- NA
lat <- na.omit(lat); lon <- na.omit(lon); swe <- na.omit(swe)

# Keep only points that fall on a valid DEM cell (i.e. on land)
points <- cbind(lon, lat)
inds   <- cellFromXY(dem_italy, points)
appo   <- dem_italy[inds]
swe[is.na(appo)] <- NA
swe[swe <= 0]    <- NA

grid_swe <- data.frame(lon = lon, lat = lat, swe = swe)
rm(lon, lat, swe); gc()

# ------------------------------------------------------------------------------
# Load and process MODIS snow cover
# ------------------------------------------------------------------------------
bbox  <- ext(XLIM[1] - 0.1, XLIM[2] + 0.1, YLIM[1] - 0.1, YLIM[2] + 0.1)
modis <- rast(MODIS_FILE)
modis <- crop(modis, bbox)
appo  <- rast(italian_dem)
appo  <- crop(appo, bbox)
compareGeom(modis, appo)
if (!is.lonlat(modis)) modis <- project(modis, "EPSG:4326")

# Keep only snow pixels (value == 1) within Italian territory
modis[modis != 1] <- NA
modis <- ifel(!is.na(appo), modis, NA)
modis <- as.factor(modis)

# ------------------------------------------------------------------------------
# Load DEM and compute hillshade
# ------------------------------------------------------------------------------
dem <- rast(DEM_FILE)
dem <- crop(dem, bbox)
if (!is.lonlat(dem)) dem <- project(dem, "EPSG:4326")

dem_smooth <- focal(dem, w = matrix(1, 7, 7), fun = mean, na.policy = "omit")
slope      <- terrain(dem_smooth, v = "slope",  unit = "radians")
aspect     <- terrain(dem_smooth, v = "aspect", unit = "radians")
hillshade  <- shade(slope, aspect, angle = SUN_ANGLE, direction = SUN_DIRECTION)
hillshade  <- (hillshade - minmax(hillshade)[1]) / (minmax(hillshade)[2] - minmax(hillshade)[1])

# Country borders
countries <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

# ------------------------------------------------------------------------------
# Plot 1: MODIS snow cover
# ------------------------------------------------------------------------------
plot_modis <- ggplot() +
  geom_spatraster(data = hillshade, maxcell = Inf) +
  scale_fill_gradient(low = "grey50", high = "grey97", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = modis, maxcell = Inf) +
  scale_fill_manual(
    values      = c("1" = "#5B9BD5"), na.value = NA, na.translate = FALSE,
    name        = "MODIS", labels = "Snow cover"
  ) +
  geom_sf(data = countries, fill = NA, color = "black", linewidth = 0.4, inherit.aes = FALSE) +
  annotation_scale(location = "br", width_hint = 0.25, style = "bar", unit_category = "metric", text_cex = 1.4) +
  coord_sf(xlim = XLIM, ylim = YLIM, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "right",
    legend.key.height = unit(1, "cm"),
    legend.title      = element_text(size = 18),
    legend.text       = element_text(size = 16),
    panel.grid        = element_line(color = "grey80", linewidth = 0.2),
    axis.text.y       = element_text(size = 16),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    plot.margin       = margin(t = 15, b = 15)
  )

# ------------------------------------------------------------------------------
# Plot 2: SWE with single colorscale — hard grey transition above 100 mm
# ------------------------------------------------------------------------------
plot_swe <- ggplot() +
  geom_spatraster(data = hillshade, maxcell = Inf) +
  scale_fill_gradient(low = "grey50", high = "grey97", guide = "none", na.value = NA) +
  new_scale_fill() +
  geom_raster(data = grid_swe, aes(x = lon, y = lat, fill = swe), na.rm = TRUE) +
  scale_fill_gradientn(
    colours  = cols_all,
    values   = vals_all,
    limits   = c(SWE_MIN, LIM_MAX),
    oob      = scales::squish,
    breaks   = seq(SWE_MIN, SWE_MAX, by = 25),
    labels   = c("0", "25", "50", "75", "100"),
    name     = "SWE [mm w.e.]",
    na.value = NA,
    guide    = guide_colorbar(
      frame.colour   = "black",
      ticks.colour   = "black",
      title.position = "top"
    )
  ) +
  geom_sf(data = countries, fill = NA, color = "black", linewidth = 0.4, inherit.aes = FALSE) +
  annotation_scale(location = "br", width_hint = 0.25, style = "bar", unit_category = "metric", text_cex = 1.4) +
  coord_sf(xlim = XLIM, ylim = YLIM, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "right",
    legend.key.height = unit(2, "cm"),
    legend.title      = element_text(size = 18, margin = margin(b = 12)),
    legend.text       = element_text(size = 16),
    panel.grid        = element_line(color = "grey80", linewidth = 0.2),
    axis.text         = element_text(size = 16),
    plot.margin       = margin(t = 15)
  )

# ------------------------------------------------------------------------------
# Combine and save
# ------------------------------------------------------------------------------
final_plot <- plot_modis / plot_swe +
  plot_annotation(
    tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  ) &
  theme(plot.tag = element_text(size = 15, face = "bold", margin = margin(r = 10)))

ggsave(
  filename = "comparison_plot.png",
  plot     = final_plot,
  width    = 12, height = 14, dpi = 300, bg = "white"
)
message("Done! Plot saved as comparison_plot.png")