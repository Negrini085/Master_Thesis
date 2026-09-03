# The main goal of this script is to make a mean SWE map from November to June for Po Basin product
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")

years <- 2011:2021
total <- numeric(0)
for(y in years){
  
  dates <- seq(as.Date(paste0(y-1, "-11-01")), as.Date(paste0(y, "-06-30")), by = "day")
  fname <- paste0("../../Po-Basin/Dataset/", y, "/SWE_" , dates, ".tif")
  
  swe_maps <- rast(fname)
  swe_maps <- clamp(swe_maps, lower = 0, values = TRUE)
  annual_map <- sum(swe_maps)/length(dates)
  
  if(y==2011) total <- annual_map
  else total <- total + annual_map
  
  cat(paste0("Taken care of ", y, "\n"))
}
total <- total/length(years)

writeRaster(total, "Dataset/mean_SWE_PO.tif", overwrite = TRUE, datatype = "FLT4S", NAflag = -9999, gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))
