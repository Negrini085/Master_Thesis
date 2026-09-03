# The main goal of this script is to create an SCD map from MODIS in order to make 
# a fair comparison with ITSNOW
rm(list = ls())
gc()

library(terra)

fname_mask <- "../../MODIS/Dataset/annual_maps/LOS/los_2020.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/MODIS_vs_PO/")

years <- 2001:2025
total <- numeric(0)
for(y in years){
  first_start <- 276
  first_end <- 365
  if((y-1)%%4==0){
    first_start <- 277
    first_end <- 366
  }
  
  if((y-1)==2000){
    first_start <- 223
    first_end <- 312
  }
  
  second_start <- 1
  second_end <- 182
  if(y%%4==0) second_end <- 183
  
  fname <- paste0("../../MODIS/Dataset/daily/", y-1,"/day_", sprintf("%03d", first_start:first_end), ".tif")
  fname <- c(fname, paste0("../../MODIS/Dataset/daily/", y,"/day_", sprintf("%03d", second_start:second_end), ".tif"))
  year_maps <- rast(fname)
  year_maps <- subst(year_maps, 2, NA)
   
   
   
  mm <- minmax(year_maps, compute = TRUE)
  if (max(mm["max", ]) > 1 || min(mm["min", ]) < 0) {
     stop("Invalid values for year ", y)
  }
  annual_map <- sum(year_maps, na.rm = TRUE)
   
  if(y==2001) total <- annual_map
  else total <- total + annual_map
   
  cat(paste0("Taken care of ", y, "\n"))
}

my_mask <- rast(fname_mask)
mask_twos <- is.na(my_mask)

total <- mask(total, mask_twos, maskvalues = 1)
total <- total/length(years)

writeRaster(total, "Dataset/mean_SCD_MODIS.tif", overwrite = TRUE, datatype = "FLT4S", NAflag = -9999, gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))