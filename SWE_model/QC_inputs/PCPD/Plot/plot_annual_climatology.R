# Annual precipitation climatology over Italian hydrological portion of the Greater 
# Alpine Region (GAR).
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

years <- 1991:2020
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/")


# Function to plot annual precipitation map
plot_annual_prec <- function(annual_prec, max_p, lon, lat, out_dir = "Images/") {
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  df_plot <- expand.grid(lon = lon, lat = lat)
  df_plot$precipitation <- as.vector(annual_prec)
  
  p <- ggplot(df_plot, aes(x = lon, y = lat, fill = precipitation)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(
      name = "Precipitation [mm]", 
      limits = c(0, max_p),
      na.value = "transparent",
      direction = -1
    ) +
    labs(
      title = "",
      x = "Longitude [°E]",
      y = "Latitude [°N]"
    ) +
    theme_minimal() +
    theme(
      plot.title      = element_text(size = 16, face = "bold"),
      axis.title.x    = element_text(size = 20, margin = margin(t = 15)),
      axis.title.y    = element_text(size = 20, margin = margin(r = 15)),
      axis.text       = element_text(size = 15),
      legend.title    = element_text(size = 20, margin = margin(b = 15)),
      legend.text     = element_text(size = 15),
      legend.key.width  = unit(1.5, "cm"),
      legend.key.height = unit(2, "cm")
    )
  
  outfile <- file.path(out_dir, "annual_prec_climatology.png")
  print(p)
  #ggsave(filename = outfile, plot = p, width = 10, height = 8, dpi = 300)
}






# Cycle over years to compute totals and annual climatology
annual_prec <- NULL
for(y in years){
  
  # Importing netCDF file
  fname <- paste0("Input/PCPD/", y, ".nc")
  nc <- nc_open(fname)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  prec <- ncvar_get(nc,"total_precipitation")
  nc_close(nc)
  
  
  # Updating annual climatology
  if (is.null(annual_prec)) {
    annual_prec <- rowSums(prec, dims = 2, na.rm = FALSE)/length(years)
  } else {
    annual_prec <- annual_prec + rowSums(prec, dims = 2, na.rm = FALSE)/length(years)
  }
  print(paste0("Correctly added ", y))
}

plot_annual_prec(annual_prec = annual_prec, max_p = max(annual_prec, na.rm = TRUE), lon = lon, lat = lat)