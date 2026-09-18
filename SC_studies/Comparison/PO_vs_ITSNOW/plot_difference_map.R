# The main goal of this script is to make a plot of the difference between mean 
# season SWE maps, in order to assess which model overestimates or underestimates 
# snow cover and where.
rm(list = ls())
gc()

library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(scales)
library(geodata)
library(ggspatial)

fname_po <- "Dataset/mean_SWE_PO.tif"
fname_itsnow <- "Dataset/mean_SWE_ITSNOW.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")



# Importing both maps and comparing their geometry. I need to project one on the 
# other: I will use Po Basin as a reference!
po_map <- rast(fname_po)
itsnow_map <- rast(fname_itsnow)
compareGeom(po_map, itsnow_map, stopOnError = FALSE)



# Projecting on Po River District map, cell sizes are almost the same. IT-snow 
# comprises the Po map. Studying what happens on intersection area.
correct_ext <- project(ext(po_map), from = crs(po_map), to = crs(itsnow_map))
po_map <- project(po_map, itsnow_map, method = "bilinear")
itsnow_map <- crop(itsnow_map, correct_ext)
po_map <- crop(po_map, correct_ext)

mask <- !is.na(itsnow_map) & !is.na(po_map)
itsnow_map[!mask] <- NA
po_map[!mask] <- NA



# Computing average swe maps ratio and difference (in order to assess eventual 
# model biases)
mask <- po_map <= 5
ratio <- itsnow_map/po_map
ratio[mask] <- NA

swe_diff <- itsnow_map - po_map
log_ratio <- log10(ratio)
names(log_ratio) <- "ratio"






# Plotting procedure
# Potting procedure
val_min <- log10(0.2)
val_max <- log10(5)
histo_min <- -200
histo_max <- 200

map_title        <- ""
map_legend_title  <- "IT-SNOW \n over \n Po District"
col_high <- "#0000FF" 
col_mid   <- "white"
col_low <- "#E00000"
map_na_color <- NA

hist_binwidth <- 5
hist_title  <- ""
hist_xlab   <- "IT-SNOW - Po District[mm w.e.]"
hist_ylab   <- "Relative frequency"
hist_fill   <- "steelblue"
hist_border <- "white"

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
italy_border <- crop(world_borders, ext(swe_diff))


p_map <- ggplot() +
  geom_spatraster(data = log_ratio, aes(fill = ratio)) +
  geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradientn(
    colours  = c(col_low, col_mid, col_high),
    values   = c(0, 0.5, 1),
    limits   = c(val_min, val_max),
    breaks   = log10(c(0.2, 0.5, 1, 2, 5)),
    labels   = c("0.2", "0.5", "1", "2", "5"),
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

vals <- values(swe_diff)[, 1]
vals <- vals[!is.na(vals)]

df_hist <- data.frame(diff = vals)
n_tot   <- nrow(df_hist)

df_hist <- df_hist[df_hist$diff >= histo_min & df_hist$diff <= histo_max, , drop = FALSE]
n_outliers <- n_tot - nrow(df_hist)

p_hist <- ggplot(df_hist, aes(x = diff)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = hist_binwidth,
    boundary = 0,
    fill = hist_fill, color = hist_border
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(limits = c(histo_min, histo_max)) +
  labs(title = NULL, x = hist_xlab, y = hist_ylab) +
  theme_minimal(base_size = size_axis_text) +
  theme(
    plot.title = element_text(size = size_title, face = "bold", hjust = 0.5),
    axis.text  = element_text(size = size_axis_text),
    axis.title = element_text(size = size_axis_title)
  )

final_plot <- p_map + p_hist + patchwork::plot_layout(nrow = 1)
print(final_plot)