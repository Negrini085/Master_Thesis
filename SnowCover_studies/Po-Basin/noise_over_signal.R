# The main goal of this script is to plot standard deviation and standard deviation
# over mean value for LOS value. This is still a plot present in MODIS paper.
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin/")
fnames <- c("Datas/swe_std_map.tif", "Datas/swe_mean_map.tif")

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
    geom_spatvector(data = italy_cropped, fill = NA, color = "black", linewidth = 0.3) +
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







# Importing raster and creating noise/signal map
los_metrics <- rast(fnames)
names(los_metrics)[1] <- "std"
names(los_metrics)[2] <- "mean"

mask_zeros <- los_metrics$mean == 0
num_zeros <- global(mask_zeros, "sum", na.rm = TRUE)$sum

print(paste0("Pixels with los equal to zero are ", num_zeros))



# Evaluating noise over signal 
appo <- los_metrics$std
appo <- mask(appo, mask_zeros, maskvalues = 1)

appo <- appo/los_metrics$mean



# Modifying raster layers and getting ready to plot
los_metrics$mean <- appo



# Plotting procedure
italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
italy_cropped <- crop(italy_border, ext(los_metrics))

custom_palette <- c("#b34d33", "#e69240", "#f0db4d", "#72e61c", "#1d8c75", "#0d4d8a")

p1 <- make_snow_plot(
  los_metrics[[1]], "Standard deviation SWE", 
  breaks = c(10, 20, 35, 55, 80),
  labels = c("0 - 10", "10 - 20", "20 - 35", "35 - 55", "55 - 80", " > 80"),
  palette = custom_palette, "SWE [mm w.e.]"
)

p2 <- make_snow_plot(
  los_metrics[[2]], "Noise over signal SWE",
  breaks = c(0.25, 0.5, 0.75, 1, 2),
  labels = c("0 - 0.25", "0.25 - 0.5", "0.5 - 0.75", "0.75 - 1.0", "1.0 - 2.0", " > 2.0"),
  palette = custom_palette, "No dim"
)

final_plot <- p1 + p2 + plot_layout(ncol = 2)
print(final_plot)