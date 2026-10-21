# The main goal of this script is to compute the mean SCD map from modis estimates, to be later compared with model outputs.
rm(list = ls())
gc()

library(terra)

fname_template <- "../Dataset/SWE_1945.nc"
fname_mask <- "../../SC_studies/MODIS/Dataset/annual_maps/LOS/los_2023.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_MODIS/")


# Importing template map
template_map <- rast(fname_template)[[1]]
my_mask <- is.na(rast(fname_mask))





# Cycle over years
years <- 2000:2023
total <- numeric(0)
for(y in years){
  start <- 1
  end <- 365
  if(y%%4==0) end <- 366
  
  if(y==2000) {start <- 221; end <- 312}
  if(y==2023) end <- 273
  
  fname <- paste0("../../SC_studies/MODIS/Dataset/daily/", y,"/day_", sprintf("%03d", start:end), ".tif")
  year_maps <- rast(fname)
  year_maps <- subst(year_maps, 2, NA)
  year_maps[my_mask] <- NA
  
  year_maps <- project(year_maps, template_map, method = "average")
  
  mm <- minmax(year_maps, compute = TRUE)
  if (max(mm["max", ]) > 1 || min(mm["min", ]) < 0) {
    stop("Invalid values for year ", y)
  }
  annual_map <- sum(year_maps, na.rm = TRUE)
  
  if(y==2000) total <- annual_map
  else total <- total + annual_map
  
  cat(paste0("Taken care of ", y, "\n"))
}




# Projecting on template map and selecting overlap region
total <- total/(length(years) - 1)
writeRaster(total, "Results/mean_SCD_MODIS.tif", overwrite = TRUE, datatype = "FLT4S", NAflag = -9999, gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))