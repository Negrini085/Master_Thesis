# The main goal of this script is to plot mean maps for snow metrics. I would like
# to reproduce actual paper plots
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS")
fnames <- c("Datas/sd_maps/sd_los.tif", "Datas/sd_maps/sd_sos.tif", "Datas/sd_maps/sd_eos.tif")

# Function to create a clean environment for plot creation
theme_paper_clean <- function() {
  theme_void() + 
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal", 
      legend.title = element_text(face = "bold", size = 9, vjust = 1),
      legend.text = element_text(size = 7),
      legend.spacing.x = unit(0.3, 'cm'), 
      legend.spacing.y = unit(0.2, 'cm'),
      plot.subtitle = element_text(face = "bold", hjust = 0.5, size = 11, margin = margin(b = 5)),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Function to actually make the plot
make_snow_plot <- function(raster_lyr, title, breaks, labels, palette, legend_name) {
  
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
    geom_spatraster(data = raster_disc) + 
    geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = palette,
      labels = labels,
      name = legend_name,
      na.value = "transparent",
      guide = guide_legend(
        ncol = 2,           
        byrow = FALSE,
        label.position = "right",
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    labs(subtitle = title) +
    theme_paper_clean()
}

# Importing raster
snow_metrics <- rast(fnames)


# Plotting procedure
italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
italy_cropped <- crop(italy_border, ext(snow_metrics))

custom_palette <- c("#b34d33", "#e69290", "#f0db4d", "#72e61c", "#1d8c75", "#0d4d8a")

p1 <- make_snow_plot(
  snow_metrics[[1]], "Standard deviation LOS", 
  breaks = c(10, 15, 20, 25, 30),
  labels = c("0 - 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", " > 30"),
  palette = custom_palette, "Days"
)

p2 <- make_snow_plot(
  snow_metrics[[2]], "Standard deviation SOS",
  breaks = c(10, 15, 20, 25, 30),
  labels = c("0 - 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", " > 30"),
  palette = custom_palette, "Days"
)

p3 <- make_snow_plot(
  snow_metrics[[3]], "Standard deviation EOS",
  breaks = c(10, 15, 20, 25, 30),
  labels = c("0 - 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", " > 30"),
  palette = custom_palette, "Days"
)

final_plot <- p1 + p2 + p3 + plot_layout(ncol = 3)
print(final_plot)