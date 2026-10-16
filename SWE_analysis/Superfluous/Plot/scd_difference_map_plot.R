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

fname_10 <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_10.nc"
fname_15 <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_15.nc"
fname_20 <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_20.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

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
  levels(raster_disc) <- data.frame(ID = 1:8, label = labels)
  
  pal_named <- setNames(palette, labels)
  
  ggplot() +
    geom_spatraster(data = raster_disc, maxcell = Inf, show.legend = TRUE) + 
    geom_spatvector(data = region_cropped, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = pal_named,
      limits = labels,
      drop = FALSE,
      name = legend_name,
      na.value = "transparent",
      na.translate = FALSE,
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
scd_20 <- rast(fname_20)
scd_20 <- mean(scd_20)

scd_15 <- rast(fname_15)
scd_15 <- mean(scd_15)

scd_10 <- rast(fname_10)
scd_10 <- mean(scd_10)

scd_15_20 <- scd_15 - scd_20
scd_15_10 <- scd_15 - scd_10
names(scd_15_20) <- "scd_diff"
names(scd_15_10) <- "scd_diff"

scd_15_20 <- ifel(scd_15_20 > 15, scd_15_20, NA)
scd_15_10 <- ifel(scd_15_10 < -15, scd_15_10, NA)


# First plot
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
region_cropped <- crop(world_borders, ext(scd_15_10))

custom_palette <- c("#b34d33", "#d66d23", "#e69125", "#f0db4d", "#72e61c", "#2ea354", "#1d8c75", "#0d4d8a")

p1 <- make_snow_plot(
  scd_15_10,
  breaks = c(-10, -5, -2.5, 0, 2.5, 5, 10),
  labels = c("< -10", "-10 - -5", "-5 - -2.5", "-2.5 - 0", "0 - 2.5", "2.5 - 5", "5 - 10", "> 10"),
  palette = custom_palette, "SCD diff. 15 - 10 \n [days]"
)





# Second plot
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(
  border_countries,
  function(iso) gadm(country = iso, level = 0, path = tempdir())
))
region_cropped <- crop(world_borders, ext(scd_15_20))

custom_palette <- c("#b34d33", "#d66d23", "#e69125", "#f0db4d", "#72e61c", "#2ea354", "#1d8c75", "#0d4d8a")

p2 <- make_snow_plot(
  scd_15_20,
  breaks = c(-10, -5, -2.5, 0, 2.5, 5, 10),
  labels = c("< -10", "-10 - -5", "-5 - -2.5", "-2.5 - 0", "0 - 2.5", "2.5 - 5", "5 - 10", "> 10"),
  palette = custom_palette, "SCD diff. 15 - 20 \n [days]"
)

p1 / p2