# The main goal of this script is to make a mean SWE map from November to June for Po Basin product
rm(list = ls())
gc()

library(terra)

years <- 2011:2021
season_maps <- vector("list", length(years))
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")


for (k in seq_along(years)) {
  y <- years[k]
  
  dates <- seq(as.Date(paste0(y - 1, "-11-01")), as.Date(paste0(y, "-06-30")), by = "day")
  fname <- paste0("../../Po-Basin/Dataset/", y, "/SWE_", dates, ".tif")
  
  ok <- file.exists(fname)
  if (!all(ok)) stop(sum(!ok), " missing files during ", y - 1, "/", y)
  
  swe_maps <- rast(fname)
  swe_maps <- clamp(swe_maps, lower = 0, values = TRUE)
  
  season_maps[[k]] <- mean(swe_maps, na.rm = TRUE)
  
  cat("Taken care of ", y, "\n")
  rm(swe_maps); gc()
}

season_stack <- rast(season_maps)
names(season_stack) <- years
mean_map <- mean(season_stack, na.rm = TRUE)

writeRaster(mean_map, "Dataset/mean_SWE_PO.tif",
            overwrite = TRUE, datatype = "FLT4S",
            gdal = c("COMPRESS=DEFLATE", "TILED=YES"))