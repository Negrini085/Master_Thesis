rm(list = ls())
gc()

library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS/")

make_los_plot <- function(fname_dem,
                          fname_los,
                          ele_step = 5,
                          probs    = c(0.05, 0.25, 0.50, 0.75, 0.95),
                          min_px   = 5, y_lab, x_lab) {
  
  dem       <- rast(fname_dem)
  mean_los  <- rast(fname_los)
  dem_masked <- mask(dem, mean_los)
  
  lims <- c(seq(from = -5, to = 4000, by = ele_step), 4700)
  class_dem <- classify(dem_masked, rcl = lims, include.lowest = TRUE)
  
  r_stack <- c(class_dem, dem_masked, mean_los)
  names(r_stack) <- c("ele_class", "elevation", "los")
  px_df <- as.data.frame(r_stack, na.rm = TRUE)
  
  percentile_df <- px_df %>%
    group_by(ele_class) %>%
    filter(n() >= min_px) %>%
    reframe(
      elevation = mean(elevation),
      prob      = probs,
      los_q     = quantile(los, probs = probs, na.rm = TRUE)
    ) %>%
    mutate(prob_lab = factor(paste0(prob * 100, "th"),
                             levels = paste0(probs * 100, "th")))
  
  ggplot(percentile_df, aes(x = elevation, y = los_q, color = prob_lab)) +
    geom_point(shape = 1, size = 1.3, stroke = 0.6, alpha = 0.75) +
    scale_color_manual(
      name   = "Percentile",
      values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#e7298a")
    ) +
    labs(
      x = x_lab,
      y = y_lab
    ) +
    theme_bw(base_size = 13) +
    theme(
      text = element_text(family = "sans", colour = "black"),
      plot.subtitle = element_text(size = 20, face = "bold", hjust = 0),
      axis.title = element_text(size = 22, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 18, colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.line = element_line(linewidth = 0.6, colour = "black"),
      axis.ticks = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks.length = unit(0.15, "cm"),
      panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 25, vjust = 0.5),
      legend.text = element_text(size = 20)
    )
}


p1 <- make_los_plot(
  fname_dem = "DEM/MODIS_dem.tif",
  fname_los = "Datas/mean_maps/mean_sos.tif",
  y_lab = "SOD [day]",
  x_lab = ""
)

p2 <- make_los_plot(
  fname_dem = "DEM/MODIS_dem.tif",
  fname_los = "Datas/mean_maps/mean_eos.tif",
  y_lab = "SED[day]",
  x_lab = "Elevation [m a.s.l.]"
)

combined <- (p1 + p2) +
  plot_layout(ncol = 1, guides = "collect") &
  plot_annotation(tag_levels = 'A')  &
  theme(legend.position = "right")

print(combined)