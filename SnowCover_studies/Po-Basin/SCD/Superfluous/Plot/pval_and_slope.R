# The main goal of this script is to reproduce a trend and slope plot that can
# be seen in MODIS paper. 
rm(list = ls())
gc()

library(terra)
library(geodata)
library(ggplot2)
library(tidyterra)
library(patchwork)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/MODIS")
fnames <- c("Datas/pval_2001_2022_los.tif", "Datas/slope_2001_2022_los.tif")


# Needed to filter on trend significance
years <- 2001:2022
files <- paste0("Dataset/annual_maps/LOS/los_", years, ".tif")

r <- rast(files)
num_years <- app(r, function(x) sum(x == 0))

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
  rcl <- NULL
  raster_disc <- NULL
  
  if(length(breaks) == 4){
    rcl <- matrix(c(
      -Inf, breaks[1], 1,
      breaks[1], breaks[2], 2,
      breaks[2], breaks[3], 3,
      breaks[3], breaks[4], 4,
      breaks[4], Inf, 5
    ), ncol = 3, byrow = TRUE)
    
    raster_disc <- classify(raster_lyr, rcl)
    raster_disc <- as.factor(raster_disc)
    levels(raster_disc) <- data.frame(ID = 1:5, label = labels)
  }
  
  else if(length(breaks) == 7){
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
  }
  
  ggplot() +
    geom_spatraster(data = raster_disc, maxcell = Inf) + 
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





# Importing raster and creating a positive/negative pval map in order to distinguish
# between positive and negative trend as it was done in Italian MODIS paper.
los_metrics <- rast(fnames)
los_metrics$`p-value` <- ifel(num_years < 5, los_metrics$`p-value`, 1)
print(global(los_metrics$`p-value`, "min", na.rm = TRUE)$min)
print(global(los_metrics$`p-value`, "max", na.rm = TRUE)$max)
los_metrics$`p-value` <- ifel(los_metrics$slope < 0, -1+los_metrics$`p-value`, 1 - los_metrics$`p-value`)



# Plotting procedure
italy_border <- gadm(country = "ITA", level = 0, path = tempdir())
italy_cropped <- crop(italy_border, ext(los_metrics))

pval_palette <- c("#8B4513","#FF0000","#E0E0E0","#87CEFA","#007FFF")
slope_palette <- c("#e31a1c", "#fd8d3c", "#feb24c", "#ffed6f", "#e2f4a6", "#abdda4", "#7bccc4","#2b8cbe")

p1 <- make_snow_plot(
  10*los_metrics[[2]], "Trend magnitude",
  breaks = c(-15, -10, -5, -2.5, 0, 2.5, 5),
  labels = c("< -15", "-15 to -10", "-10 to -5", "-5 to -2.5", "-2.5 to 0", "0 to 2.5", "2.5 to 5", "> 5"),
  palette = slope_palette, "Days/Decade"
)

p2 <- make_snow_plot(
  los_metrics[[1]], "Trend sign and significance",
  breaks = c(-0.95, -0.9, 0.9, 0.95),
  labels = c("< 0 (95% confidence)", "< 0 (90% confidence)", "Not significant", "> 0 (90% confidence)", "> 0 (95% confidence)"),
  palette = pval_palette, "Confidence"
)

final_plot <- p1 + p2 + plot_layout(ncol = 2)
print(final_plot)