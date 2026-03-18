# The main goal of this script is to create annual swe maps. There are 62 maps, 
# which cannot be plot in a compact way. I feel like I will just plot 60 of them
# in order to be sure that they meet our standards.
rm(list = ls())
gc()

library(ncdf4)
library(scales)
library(ggplot2)
library(patchwork)

years <- 1962:2021
fname <- "Datas/hydrological_maps.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")


# Function to create a single subplot
make_plot <- function(layer_idx, year, show_x = FALSE, show_y = FALSE) {
  grid <- expand.grid(E = lon, N = lat)
  grid$swe <- as.vector(map[, , layer_idx])
  grid <- grid[!is.nan(grid$swe), ]
  
  ggplot(grid, aes(x = E, y = N, fill = swe)) +
    geom_raster() +
    scale_fill_viridis_c(
      option = "viridis", direction = 1, name = "SWE [m w.e]",
      limits = c(0, 1), oob = squish
    ) +
    coord_equal() +
    scale_x_continuous(labels = ~ paste0(.x / 1000, " km")) +
    scale_y_continuous(labels = ~ paste0(.x / 1000, " km")) +
    labs(
      title = paste0(year),
      x = if(show_x) "E [km, LV95]" else "",
      y = if(show_y) "N [km, LV95]" else ""
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text.x = if(show_x) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.text.y = if(show_y) element_text() else element_blank(),
      legend.position = "none"
    )
}




# Getting variables to plot
nc <- nc_open(fname)
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
map <- ncvar_get(nc, "SWE")
nc_close(nc)

# Cycle over plots
for (p in 1:4) {
  idx_start <- (p - 1) * 15 + 1
  idx_end   <- p * 15
  
  plots <- lapply(1:15, function(i) {
    col <- ((i - 1) %% 5) + 1
    row <- ((i - 1) %/% 5) + 1 
    
    show_y <- (col == 1)
    show_x <- (row == 3)
    
    make_plot(idx_start + i - 1, years[idx_start + i - 1], show_x, show_y)
  })
  
  combined <- wrap_plots(plots, nrow = 3, ncol = 5) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  fname_out <- paste0("Images/swe_maps_", years[idx_start], "_", years[idx_end], ".png")
  ggsave(fname_out, combined, width = 20, height = 10, dpi = 150)
  cat("Salvato:", fname_out, "\n")
}