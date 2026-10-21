# The main goal of this script is to make a plot of the difference between mean 
# season SWE maps, in order to assess which model overestimates or underestimates 
# snow cover and where.
rm(list = ls())
gc()

library(terra)
library(scales)
library(ggplot2)
library(geodata)
library(tidyterra)
library(patchwork)
library(ggspatial)

fname_itsnow <- "Results/swe_climatologies_itsnow.tif"
fname_model <- "Results/swe_climatologies_model.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")



# Function to make ratio plot
plot_ratio_map <- function(
    r, title = NULL, borders = NULL, show_x = TRUE, show_y = TRUE, breaks = c(0.2, 0.5, 1, 2, 5), 
    val_min = 0.2, val_max = 5, col_low = "#E00000", col_mid = "white", col_high = "#0000FF", na_color = NA,
    border_color = "black", border_width = 0.4, legend_title = "ITSNOW \n over \n model",
    x_lab = "Longitude [°E]", y_lab = "Latitude [°N]", size_title = 15, size_axis_title = 20,
    size_axis_text = 15, size_legend_title = 20, size_legend_text = 15) {
  
  r <- log10(r)
  names(r) <- "value"
  
  lim <- log10(c(val_min, val_max))
  mid <- (0 - lim[1]) / (lim[2] - lim[1])
  
  p <- ggplot() +
    geom_spatraster(data = r, aes(fill = value)) +
    scale_fill_gradientn(
      colours  = c(col_low, col_mid, col_high),
      values   = c(0, mid, 1),
      limits   = lim,
      breaks   = log10(breaks),
      labels   = format(breaks, drop0trailing = TRUE, trim = TRUE),
      oob      = function(x, range) scales::squish(x, range, only.finite = FALSE),
      na.value = na_color,
      name     = legend_title
    ) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_minimal(base_size = size_axis_text) +
    theme(
      plot.title      = element_text(size = size_title, face = "bold", hjust = 0.5),
      axis.text       = element_text(size = size_axis_text),
      axis.title      = element_text(size = size_axis_title),
      legend.title    = element_text(size = size_legend_title),
      legend.text     = element_text(size = size_legend_text),
      legend.position = "right"
    )
  
  if (!is.null(borders)) {
    p <- p + geom_spatvector(data = borders, fill = NA,
                             color = border_color, linewidth = border_width)
  }
  
  p <- p + coord_sf(expand = FALSE)
  if (!show_x) p <- p + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  if (!show_y) p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  
  p
}



# Importing both raster and comparing their geometry
itsnow_maps <- rast(fname_itsnow)
model_maps <- rast(fname_model)
compareGeom(itsnow_maps, model_maps, stopOnError = TRUE)

# Checking whether the overlap is exact or not
overlap_check <- !is.na(model_maps) == !is.na(itsnow_maps)
global(!overlap_check, "sum", na.rm = TRUE)


# Computing average swe maps ratio
mask <- model_maps <= 5
ratio <- itsnow_maps/model_maps
ratio[mask] <- NA
names(ratio)





# Plotting procedure
border_countries <- c("ITA", "FRA", "CHE", "AUT", "SVN", "DEU", "HRV")
world_borders <- vect(lapply(border_countries, function(iso) gadm(country = iso, level = 0, path = tempdir())))
italy_border <- crop(project(world_borders, crs(ratio)), ext(ratio))

n_col  <- 2
months <- names(ratio)

maps <- lapply(seq_along(months), function(i) {
  plot_ratio_map(
    ratio[[i]], title   = paste("1", months[i]), borders = italy_border,
    show_x  = i > length(months) - n_col, show_y  = (i - 1) %% n_col == 0
  )
})

final_map <- wrap_plots(maps, ncol = n_col) +
  plot_layout(guides = "collect")

final_map

# ggsave("Results/swe_ratio_maps.png", final_map, width = 14, height = 12, dpi = 300, bg = "white")