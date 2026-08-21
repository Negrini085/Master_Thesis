# The main goal of this script is to open a DEM to Northern Italy in order to create 
# some plots for a chapter of my master thesis
rm(list = ls())
gc()

library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Study_area/")
fname_map <- "DEM/study_area_dem.tif"

dem <- rast(fname_map)

cat("Number of cells:", ncell(dem), "\n")
cat("Resolution:", paste(res(dem), collapse = " x "), "\n")
cat("Estimated RAM if materialized as data.frame (GB):",
    round(ncell(dem) * 8 * 3 / 1e9, 2), "\n")

elev_range <- as.numeric(minmax(dem)[, 1])

p_map <- ggplot() +
  geom_spatraster(data = dem) +
  scale_fill_gradientn(
    colours  = terrain.colors(100),
    limits   = elev_range,
    name     = "Elevation\n(m a.s.l.)",
    na.value = "transparent"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title    = "",
    subtitle = "",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "grey30"),
    panel.grid      = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "right"
  )

elev_vals <- values(dem, na.rm = TRUE)
elev_df <- data.frame(elevation = as.vector(elev_vals))
rm(elev_vals); gc()


p_hist <- ggplot(elev_df, aes(x = elevation, fill = after_stat(x))) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 100,
    color = "white",
    linewidth = 0.1
  ) +
  scale_fill_gradientn(
    colours = terrain.colors(100),
    limits = elev_range,
    guide = "none"
  ) +
  scale_x_continuous(limits = elev_range) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "",
    x = "Elevation (m a.s.l.)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  )

rm(elev_df); gc()

final_plot <- p_map + p_hist +
  plot_layout(widths = c(1.5, 1.5))

print(final_plot)

ggsave(
  filename = "Images/study_area.png",
  plot     = final_plot,
  width    = 14, height = 6, dpi = 300, bg = "white"
)

rm(dem, final_plot); gc()