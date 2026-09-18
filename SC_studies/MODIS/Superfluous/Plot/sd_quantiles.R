# The main goal of this script is to plot standard deviation and standard deviation
# over mean value for LOS value. This is still a plot present in MODIS paper.
rm(list = ls())
gc()

library(dplyr)
library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

fname_dem <- "DEM/MODIS_dem.tif"
fname_sd <- "Datas/sd_maps/sd_sos.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS")

# Function to create a clean environment for plot creation
theme_paper_clean_map <- function() {
  theme_void() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.00, -0.1),
      legend.justification = c(0, 0),
      legend.direction = "vertical",
      # legend.background = element_rect(fill = alpha("white", 0.75),
      #                                  colour = "grey70", linewidth = 0.2),
      # legend.margin = margin(5, 8, 5, 8),
      legend.key.size = unit(0.38, "cm"),
      legend.title = element_text(face = "bold", size = 20, hjust = 0),
      legend.text = element_text(size = 15),
      legend.spacing.x = unit(1, "cm"),
      plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 25, margin = margin(b = 5)),
      plot.margin = margin(5, 5, 5, 5)
    )
}


# Function to actually make the plot
make_snow_plot_map <- function(raster_lyr, title, breaks, labels, palette, legend_name) {
  
  rcl <- matrix(c(
    -Inf, breaks[1], 1,
    breaks[1], breaks[2], 2,
    breaks[2], breaks[3], 3,
    breaks[3], breaks[4], 4,
    breaks[4], breaks[5], 5,
    breaks[5], Inf, 6
  ), ncol = 3, byrow = TRUE)
  
  raster_disc <- classify(raster_lyr, rcl)
  raster_disc <- as.factor(raster_disc)
  levels(raster_disc) <- data.frame(ID = 1:6, label = labels)
  
  ggplot() +
    geom_spatraster(data = raster_disc, maxcell = Inf) + 
    geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = palette,
      labels = labels,
      name = legend_name,
      na.value = "transparent",
      na.translate = FALSE,
      guide = guide_legend(
        title.position = "top",
        ncol = 2,           
        byrow = FALSE,
        label.position = "right",
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    labs(subtitle = title) +
    theme_paper_clean_map()
}







# Importing standard deviation and DEM
dem <- rast(fname_dem)
sd_map <- rast(fname_sd)
dem_masked <- mask(dem, sd_map)


# Classifying DEM pixels based on their elevation
ele_step <- 5
lims <- c(seq(from = -5, to = 4000, by = ele_step), 4700)
class_dem <- classify(dem_masked, rcl = lims, include.lowest = TRUE)


# Computing percentiles
r_stack <- c(class_dem, dem_masked, sd_map)
names(r_stack) <- c("ele_class", "elevation", "sd")
px_df <- as.data.frame(r_stack, na.rm = TRUE)

probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
min_px <- 5

percentile_df <- px_df %>%
  group_by(ele_class) %>%
  filter(n() >= min_px) %>%
  reframe(
    elevation = mean(elevation),
    prob      = probs,
    sd_q     = quantile(sd, probs = probs, na.rm = TRUE)
  ) %>%
  mutate(prob_lab = factor(paste0(prob * 100, "th"),
                           levels = paste0(probs * 100, "th")))



# Plotting procedure
italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
italy_cropped <- crop(italy_border, ext(sd_map))

custom_palette <- c("#b34d33", "#e69240", "#f0db4d", "#72e61c", "#1d8c75", "#0d4d8a")

p1 <- make_snow_plot_map(
  sd_map, "", 
  breaks = c(3, 8, 13, 18, 23),
  labels = c("0 - 3", "3 - 8", "8 - 13", "13 - 18", "18 - 23", " > 23"),
  palette = custom_palette, "SOD standard \ndeviation [days]"
)

p2 <- ggplot(percentile_df, aes(x = elevation, y = sd_q, color = prob_lab)) +
  geom_point(shape = 1, size = 1.3, stroke = 0.6, alpha = 0.75) +
  scale_color_manual(
    name   = "Percentile",
    values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#e7298a")
  ) +
  labs(
    x = "Elevation [m a.s.l.]",
    y = "SOD standard deviation [days]"
  ) + 
  guides(color = guide_legend(override.aes = list(size = 3.5, stroke = 1, alpha = 1))) +
  theme_bw(base_size = 13) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 22,
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    axis.text = element_text(
      size = 18,
      colour = "black"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.line = element_line(
      linewidth = 0.6,
      colour = "black"
    ),
    axis.ticks = element_line(
      linewidth = 0.5,
      colour = "black"
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    ),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.75),
                                     colour = "grey70", linewidth = 0.2),
    legend.key = element_blank(),
    legend.key.size = unit(0.55, "cm"),
    legend.margin = margin(6, 9, 6, 9),
    legend.title = element_text(face = "bold", size = 17, hjust = 0),
    legend.text = element_text(size = 14)
  )


fname_dem <- "DEM/MODIS_dem.tif"
fname_sd <- "Datas/sd_maps/sd_eos.tif"


# Importing standard deviation and DEM
dem <- rast(fname_dem)
sd_map <- rast(fname_sd)
dem_masked <- mask(dem, sd_map)


# Classifying DEM pixels based on their elevation
ele_step <- 5
lims <- c(seq(from = -5, to = 4000, by = ele_step), 4700)
class_dem <- classify(dem_masked, rcl = lims, include.lowest = TRUE)


# Computing percentiles
r_stack <- c(class_dem, dem_masked, sd_map)
names(r_stack) <- c("ele_class", "elevation", "sd")
px_df <- as.data.frame(r_stack, na.rm = TRUE)

probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
min_px <- 5

percentile_df <- px_df %>%
  group_by(ele_class) %>%
  filter(n() >= min_px) %>%
  reframe(
    elevation = mean(elevation),
    prob      = probs,
    sd_q     = quantile(sd, probs = probs, na.rm = TRUE)
  ) %>%
  mutate(prob_lab = factor(paste0(prob * 100, "th"),
                           levels = paste0(probs * 100, "th")))


p3 <- make_snow_plot_map(
  sd_map, "", 
  breaks = c(3, 8, 13, 18, 23),
  labels = c("0 - 3", "3 - 8", "8 - 13", "13 - 18", "18 - 23", " > 23"),
  palette = custom_palette, "SED standard \ndeviation [days]"
)

p4 <- ggplot(percentile_df, aes(x = elevation, y = sd_q, color = prob_lab)) +
  geom_point(shape = 1, size = 1.3, stroke = 0.6, alpha = 0.75) +
  scale_color_manual(
    name   = "Percentile",
    values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#e7298a")
  ) +
  labs(
    x = "Elevation [m a.s.l.]",
    y = "SED standard deviation [days]"
  ) + 
  guides(color = guide_legend(override.aes = list(size = 3.5, stroke = 1, alpha = 1))) +
  theme_bw(base_size = 13) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 22,
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    axis.text = element_text(
      size = 18,
      colour = "black"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.line = element_line(
      linewidth = 0.6,
      colour = "black"
    ),
    axis.ticks = element_line(
      linewidth = 0.5,
      colour = "black"
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    ),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.75),
                                     colour = "grey70", linewidth = 0.2),
    legend.key = element_blank(),
    legend.key.size = unit(0.55, "cm"),
    legend.margin = margin(6, 9, 6, 9),
    legend.title = element_text(face = "bold", size = 17, hjust = 0),
    legend.text = element_text(size = 14)
  )




final_plot <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, widths = c(1, 1.5)) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 20, face = "bold"),
    plot.tag.position = c(0.05, 0.95)
  )
print(final_plot)