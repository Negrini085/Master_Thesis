# The main goal of this script is to make scd maps for PO snow product
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/MODIS_vs_PO/")

years <- 2001:2021
total <- numeric(0)
for(y in years){
  
  dates <- seq(as.Date(paste0(y-1, "-10-03")), as.Date(paste0(y, "-07-01")), by = "day")
  fname <- paste0("../../Po-Basin/Dataset/", y, "/SWE_" , dates, ".tif")
  swe_maps <- rast(fname)
  
  
  year_maps <- ifel(swe_maps > 0, 1, 0)


  mm <- minmax(year_maps, compute = TRUE)
  if (max(mm["max", ]) > 1 || min(mm["min", ]) < 0) {
    stop("Invalid values for year ", y)
  }
  annual_map <- sum(year_maps, na.rm = TRUE)

  if(y==2001) total <- annual_map
  else total <- total + annual_map

  cat(paste0("Taken care of ", y, "\n"))
}
total <- total/length(years)

writeRaster(total, "Dataset/mean_SCD_PO.tif", overwrite = TRUE, datatype = "FLT4S", NAflag = -9999, gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))