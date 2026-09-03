# The goal of this script is to make a plot to compare SCD results between ITSNOW and MODIS.
rm(list = ls())
gc()

library(terra)
library(ncdf4)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(scales)
library(geodata)

fname_DEM    <- "Dataset/DEM_Italy.tif"
fname_MODIS  <- "Dataset/scd_MODIS.tif"
fname_ITSNOW <- "Dataset/scd_ITSNOW.nc"

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/MODIS_vs_ITSNOW/")


# Importing files and makng projections
modis  <- rast(fname_MODIS)
itsnow <- rast(fname_ITSNOW)
dem <- rast(fname_DEM)

itsnow <- project(itsnow, modis, method = "bilinear")
dem <- project(dem, modis, method = "bilinear")


# Making difference
diff <- modis - itsnow
mask <- is.na(dem)
diff <- mask(diff, mask, maskvalues = TRUE, updatevalue = NA)

names(diff) <- "diff"










# Potting procedure
val_min <- -100
val_max <- 100

map_title        <- "SCD difference (MODIS - ITSNOW)"
map_legend_title  <- "SCD diff.\n[Days]"
col_low   <- "#2166AC"
col_mid   <- "white"
col_high  <- "#B2182B"
map_na_color <- NA

hist_binwidth <- 2
hist_title  <- "SCD difference distribution"
hist_xlab   <- "SCD difference [days]"
hist_ylab   <- "Relative frequency"
hist_fill   <- "steelblue"
hist_border <- "white"

size_title        <- 16
size_axis_title   <- 20
size_axis_text    <- 15
size_legend_title <- 20
size_legend_text  <- 15

italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
italy_border <- project(italy_border, diff)

p_map <- ggplot() +
  geom_spatraster(data = diff, aes(fill = diff)) +
  geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradient2(
    low = col_low, mid = col_mid, high = col_high, midpoint = 0,
    limits = c(val_min, val_max),
    oob = scales::squish,
    na.value = map_na_color,
    name = map_legend_title
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

vals <- values(diff)[, 1]
vals <- vals[!is.na(vals)]

df_hist <- data.frame(diff = vals)
n_tot   <- nrow(df_hist)

df_hist <- df_hist[df_hist$diff >= val_min & df_hist$diff <= val_max, , drop = FALSE]
n_outliers <- n_tot - nrow(df_hist)

p_hist <- ggplot(df_hist, aes(x = diff)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = hist_binwidth,
    boundary = 0,
    fill = hist_fill, color = hist_border
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(limits = c(val_min, val_max)) +
  labs(title = NULL, x = hist_xlab, y = hist_ylab) +
  theme_minimal(base_size = size_axis_text) +
  theme(
    plot.title = element_text(size = size_title, face = "bold", hjust = 0.5),
    axis.text  = element_text(size = size_axis_text),
    axis.title = element_text(size = size_axis_title)
  )

final_plot <- p_map + p_hist + patchwork::plot_layout(nrow = 1)
print(final_plot)