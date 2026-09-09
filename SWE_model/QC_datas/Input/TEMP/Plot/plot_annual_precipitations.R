# The main goal of this script is to compute and plot annual precipitations, in 
# order to also understand the extent of these grids.
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Input/TEMP/")
years <- 1951:2023

# Function to find colormap limits
find_total_min_max <- function(years){
  max_p <- 0
  min_p <- 0
  
  for(y in years){
    fname <- paste0("../../../Input/TEMP/temperatures_", y, ".nc")
    if(!file.exists(fname)) stop(paste0("No temperature file for ", y))
    
    nc <- nc_open(fname)
    tmax <- ncvar_get(nc,"tmxd", start = c(1, 1, 1), count = c(-1, -1, 1))
    nc_close(nc)
    
    appo_min <- min(tmax, na.rm = TRUE)
    appo_max <- max(tmax, na.rm = TRUE)
    if(appo_max > max_p) max_p <- appo_max
    if(appo_min < min_p) min_p <- appo_min
  }
  
  return(c(min_p, max_p))
}

# Function to plot annual precipitation map
plot_daily_tmax <- function(tmax, lims, lon, lat, year, out_dir = "Images/") {
  
  # Creating repo if not existent
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # Dataframe creation
  df_plot <- expand.grid(lon = lon, lat = lat)
  df_plot$tmax <- as.vector(tmax)
  
  # Plotting procedure
  p <- ggplot(df_plot, aes(x = lon, y = lat, fill = tmax)) +
    geom_raster() +
    coord_quickmap() +
    scale_fill_viridis_c(
      name = "Max. Temperature", 
      limits = lims
    ) +
    labs(
      title = paste0("Max. Temperature 01-01-", year),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal()
  
  # Saving plot
  outfile <- file.path(out_dir, paste0(year, ".png"))
  ggsave(filename = outfile, plot = p, width = 10, height = 8, dpi = 300)
}






# Finding maximum total precipitation value
lims <- find_total_min_max(years = years)
print("Maximum temperature value found!")

# Cycle over years
for(y in years){
  fname <- paste0("../../../Input/TEMP/temperatures_", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No temperature file for ", y))
  
  # Opening precipitation maps
  nc <- nc_open(fname)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  tmax <- ncvar_get(nc,"tmxd", start = c(1, 1, 1), count = c(-1, -1, 1))
  nc_close(nc)
  
  # Annual values and plot
  plot_daily_tmax(tmax = tmax, lims = lims, lon = lon, lat = lat, year = y)
  print(paste0("Made plot for ", y, "!"))
}