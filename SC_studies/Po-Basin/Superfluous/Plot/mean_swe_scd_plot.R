# The main goal of this script is to create a plot to compare SCD and SWE mean distributions.
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(patchwork)

fname_DEM <- "DEM/DEM_Italy.tif"
fname_SWE <- "Datas/swe_seasonal_maps.nc"
fname_SCD <- "SCD/SCD-from-RHO/Datas/scd_mean_map.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/IT-Snow/")


# Function to create a clean environment for plot creation
theme_paper_clean <- function() {
  theme_void() + 
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal", 
      legend.title = element_text(face = "bold", size = 20, vjust = 0.5),
      legend.text = element_text(size = 15),
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
    geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_manual(
      values = palette,
      labels = labels,
      name = legend_name,
      na.value = "transparent",
      guide = guide_legend(
        title.position = "top",
        ncol = 2,           
        byrow = FALSE,
        label.position = "right",
        keywidth = unit(0.4, "cm"),
        keyheight = unit(0.4, "cm")
      )
    ) +
    theme_paper_clean()
}

make_snow_conti_plot <- function(raster_lyr, limits, legend_name, low = "white", high = "#08306B") {
  
  ggplot() +
    geom_spatraster(data = raster_lyr, maxcell = Inf) +
    geom_spatvector(data = italy_border, fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_gradient(
      low       = low,
      high      = high,
      limits    = limits,
      oob       = scales::squish,
      na.value  = "transparent",
      name      = legend_name,
      breaks    = scales::pretty_breaks(5),
      guide     = guide_colorbar(
        title.position = "top",
        barwidth       = unit(5, "cm"),
        barheight      = unit(0.5, "cm"),
        frame.colour   = "black",
        ticks.colour   = "black"
      )
    ) +
    theme_paper_clean()
}


# Importing both dataset and dem
mean_swe <- rast(fname_SWE, subds = "SWE")
mean_swe <- mean(mean_swe, na.rm = TRUE)
mean_scd <- rast(fname_SCD)
dem <- rast(fname_DEM)


# Setting to zero missing data
dem <- project(dem, mean_scd, method = "bilinear")
mask_cond <- is.na(dem)
mean_swe <- mask(mean_swe, mask_cond, maskvalues = TRUE, updatevalue = NA)

mask_cond <- is.na(dem)
mean_scd <- mask(mean_scd, mask_cond, maskvalues = TRUE, updatevalue = NA)


# Plotting procedure
italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
italy_cropped <- crop(italy_border, ext(mean_scd))

scd_palette <- c("#b34d33", "#d66d23", "#e69125", "#f0db4d", "#72e61c", "#2ea354", "#1d8c75", "#0d4d8a")
swe_palette <- c("#FFFFFF", "#EAF3FB", "#CFE3F4", "#A6CEE9", "#6BAED6", "#3B8BC4", "#1B62A8", "#08306B")

p1 <- make_snow_plot(
  mean_swe,
  breaks = c(0, 3, 9, 27, 81, 243, 729),
  labels = c("0", "0 - 3", "3 - 9", "9 - 27", "27 - 81", "81 - 243", "243 - 729", " > 729"),
  palette = swe_palette, "Average SWE [mm]"
)

p2 <- make_snow_plot(
  mean_scd,
  breaks = c(11, 39, 78, 123, 167, 211, 278),
  labels = c("0 - 11", "12 - 39", "40 - 78", "79 - 123", "124 - 167", "168 - 211", "212 - 278", "279 - 365"),
  palette = scd_palette, "Average SCD [days]"
)

final_plot <- p1 + p2 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 12, face = "bold"),
    plot.tag.position = c(0.05, 0.95)
  )
print(final_plot)