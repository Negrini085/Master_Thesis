# The main goal of this script is to compute and plot annual precipitations, in 
# order to also understand the extent of these grids.
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Input/PCPD/")
years <- 1951:2023

# Function to find colormap limits
find_total_max <- function(years){
  max_p <- 0
  
  for(y in years){
    fname <- paste0("../../../Input/PCPD/", y, ".nc")
    if(!file.exists(fname)) stop(paste0("No precipitation file for ", y))
    
    nc <- nc_open(fname)
    prec <- ncvar_get(nc,"total_precipitation")
    nc_close(nc)
    
    appo <- rowSums(prec, dims = 2, na.rm = FALSE)
    appo <- max(appo, na.rm = TRUE)
    
    if(appo > max_p) max_p <- appo
  }
  
  return(max_p)
}

# Function to plot annual precipitation map
plot_annual_prec <- function(annual_prec, max_p, lon, lat, year, out_dir = "Images/") {
  
  # Creating repo if not existent
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # Dataframe creation
  df_plot <- expand.grid(lon = lon, lat = lat)
  df_plot$precipitation <- as.vector(annual_prec)
  
  # Plotting procedure
  p <- ggplot(df_plot, aes(x = lon, y = lat, fill = precipitation)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(
      name = "Precipitation", 
      limits = c(0, max_p)
    ) +
    labs(
      title = paste("Annual precipitation", year),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal()
  
  # Saving plot
  outfile <- file.path(out_dir, paste0(year, ".png"))
  ggsave(filename = outfile, plot = p, width = 10, height = 8, dpi = 300)
}






# Finding maximum total precipitation value
max_p <- find_total_max(years = years)
print("Maximum total precipitation value found!")

# Cycle over years
for(y in years){
  fname <- paste0("../../../Input/PCPD/", y, ".nc")
  if(!file.exists(fname)) stop(paste0("No precipitation file for ", y))
  
  # Opening precipitation maps
  nc <- nc_open(fname)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  prec <- ncvar_get(nc,"total_precipitation")
  nc_close(nc)
  
  # Annual values and plot
  annual_prec <- rowSums(prec, dims = 2, na.rm = FALSE)
  plot_annual_prec(annual_prec = annual_prec, max_p = max_p, lon = lon, lat = lat, year = y)
  print(paste0("Made plot for ", y, "!"))
}