# The main goal of this script is to create a plot of the average lenght of season 
# according to MODIS binary fields.
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")
fname <- "Results/scd_maps_hydro_1952_to_2023.nc"

# Function to create a clean environment for plot creation
theme_paper_clean <- function() {
  theme_void() + 
    theme(
      legend.position = "right",
      legend.direction = "horizontal", 
      legend.title = element_text(face = "bold", size = 25, vjust = 0.5),
      legend.text = element_text(size = 20),
      legend.spacing.x = unit(0.3, 'cm'), 
      legend.spacing.y = unit(0.2, 'cm'),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Function to actually make the plot
make_snow_plot <- function(raster_lyr, breaks, labels, palette, legend_name) {
  
  rcl <- matrix(c(
    -Inf, breaks[1], 1,
    breaks[1], breaks[2], 2,
    breaks[2], breaks[3], 3,
    breaks[3], breaks[4], 4,
    breaks[4], breaks[5], 5,
    breaks[5], breaks[6], 6,
    breaks[6], breaks[7], 7,
    breaks[7], Inf, 8
  ), ncol = 3, byrow = TRUE)
  
  raster_disc <- classify(raster_lyr, rcl)
  raster_disc <- as.factor(raster_disc)
  levels(raster_disc) <- data.frame(ID = 1:8, label = labels)
  
  ggplot() +
    geom_spatraster(data = raster_disc, maxcell = Inf) + 
    geom_spatvector(data = region_cropped, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = palette,
      labels = labels,
      name = legend_name,
      na.value = "transparent",
      guide = guide_legend(
        title.position = "top",
        ncol = 1,           
        byrow = FALSE,
        label.position = "right",
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    theme_paper_clean()
}





# Importing raster
snow_metrics <- rast(fname)
scd_mean <- mean(snow_metrics)
names(scd_mean) <- "scd_mean"


# Plotting procedure
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
region_cropped <- crop(world_borders, ext(scd_mean))

custom_palette <- c("#b34d33", "#d66d23", "#e69125", "#f0db4d", "#72e61c", "#2ea354", "#1d8c75", "#0d4d8a")

p1 <- make_snow_plot(
  scd_mean,
  breaks = c(11, 39, 78, 123, 167, 211, 278),
  labels = c("0 - 11", "12 - 39", "40 - 78", "79 - 123", "124 - 167", "168 - 211", "212 - 278", "279 - 365"),
  palette = custom_palette, "Average SCD (days)"
)

print(p1)