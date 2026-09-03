# The main goal of this script is to create an SCD map from MODIS in order to make 
# a fair comparison with ITSNOW
rm(list = ls())
gc()

library(terra)

fname_mask <- "../../MODIS/Dataset/annual_maps/LOS/los_2020.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/MODIS_vs_ITSNOW/")

years <- 2010:2025
total <- numeric(0)
for(y in years){
  start <- 1
  end <- 365
  if(y%%4==0) end <- 366
  
  if(y==2010) start <- 244
  if(y==2025) end <- 243
  
  fname <- paste0("../../MODIS/Dataset/daily/", y,"/day_", sprintf("%03d", start:end), ".tif")
  year_maps <- rast(fname)
  year_maps <- subst(year_maps, 2, NA)
  
  
  
  mm <- minmax(year_maps, compute = TRUE)
  if (max(mm["max", ]) > 1 || min(mm["min", ]) < 0) {
    stop("Invalid values for year ", y)
  }
  annual_map <- sum(year_maps, na.rm = TRUE)

  if(y==2010) total <- annual_map
  else total <- total + annual_map
  
  cat(paste0("Taken care of ", y, "\n"))
}

my_mask <- rast(fname_mask)
mask_twos <- is.na(my_mask)

total <- mask(total, mask_twos, maskvalues = 1)
total <- total/(length(years) - 1)

writeRaster(total, "mean_SCD_MODIS.tif", overwrite = TRUE, datatype = "FLT4S", NAflag = -9999, gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))