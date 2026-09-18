# The main goal of this script is to crop PO Basin daily maps in order to make accurate SWE estimations.
rm(list = ls())
gc()

library(terra)

years <- 2011:2021
fname_mask <- "Dataset/DEM_Italy.tif"
fname_ext <- "Dataset/mean_SWE_PO.tif"
fname_proj <- "Dataset/mean_SWE_ITSNOW.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")


# Importing rasters that I need in order to compute SWE total volumes
dem <- rast(fname_mask)
extent_map <- rast(fname_ext)
itsnow_map <- rast(fname_proj)
dem <- project(dem, itsnow_map, method = "bilinear")
correct_ext <-  project(ext(extent_map), from = crs(extent_map), to = crs(itsnow_map))

for(y in years){
  
  # Selecting filenames
  dates_first <-seq(as.Date(paste0(y-1, "-10-03")), as.Date(paste0(y-1, "-12-31")))
  fnames_first <- paste0("../../Po-Basin/Dataset/", y, "/SWE_", dates_first, ".tif")
  
  dates_second <-seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-07-01")))
  fnames_second <- paste0("../../Po-Basin/Dataset/", y, "/SWE_", dates_second, ".tif")
  fnames <- c(fnames_first, fnames_second)
  
  
  # Importing swe maps and projecting over ITSNOW map
  r <- rast(fnames)
  r <- project(r, itsnow_map, method = "bilinear")
  
  # Masking in order to compute SWE metrics only on common area
  compareGeom(dem, r, stopOnError = TRUE)
  mask <- !is.na(dem) & !is.na(r)
  r[!mask] <- NA
  r <- crop(r, correct_ext)
  
  outnames <- file.path("Dataset/Daily/Po", paste0("SWE_", c(dates_first, dates_second), ".tif"))
  writeRaster(
    r, filename  = outnames, overwrite = TRUE,
    datatype  = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES")
  )
  cat("Done for year ", y, "\n")
}