# The main goal of this script is to assess whether a relationship between years 
# with continuous snow cover and elevation exists or not.
rm(list = ls())
gc()

library(patchwork)
library(ggplot2)
library(terra)

fname_dem <- "DEM/DEM_compatible.tif"
fname_map <- "Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")


# Plot parameters
bin_width  <- 100
min_pixels <- 25
drop_zeros <- FALSE
max_points <- 2e5
save_png   <- TRUE


# Importing DEM and continuous snow cover duration maps
map <- rast(fname_map)
dem <- rast(fname_dem)

map[map < 0] <- NA


# Extracting cell values
df <- data.frame(
  elev = values(dem, mat = FALSE),
  yrs  = values(map, mat = FALSE)
)
df <- na.omit(df)


# Left panel: cell-wise scatter plot
p1 <- ggplot(df, aes(x = elev, y = yrs)) +
  geom_point(colour = "#2C7FB8", alpha = 0.20, size = 0.5) +
  # stat_summary_bin(fun = mean, bins = 40, geom = "line",
  #                  colour = "#B2182B", linewidth = 0.8) +
  labs(
    title    = "Cell-wise relationship",
    x        = "Elevation (m a.s.l.)",
    y        = "# years CSC"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


# Right panel: elevation distribution of persistently snow-covered cells
p2 <- ggplot(subset(df, yrs > 0), aes(x = elev)) +
  geom_histogram(binwidth = 100, fill = "#2C7FB8", colour = "white") +
  labs(
    title    = "Elevation distribution",
    x        = "Elevation (m a.s.l.)",
    y        = "Number of grid cells"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


p1 + p2

ggsave("Images/elevation_vs_continuous_snow.png", p1 + p2,
       width = 11, height = 4.5, dpi = 300, bg = "white")