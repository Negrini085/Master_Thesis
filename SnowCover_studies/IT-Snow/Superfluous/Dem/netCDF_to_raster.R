# Convert NetCDF into a GeoTIFF using terra, in order to keep grid & extent consistent
rm(list = ls())
gc()

library(terra)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

ncfile  <- "y2011/ITSNOW_SWE_201103.nc"
varname <- "SWE"
outfile <- "SWE_map.tif"


# Open the target variable as a SpatRaster
r <- rast(ncfile, subds = varname)


# Ensure CRS is set (only if missing/empty)
if (is.na(crs(r)) || crs(r) == "") crs(r) <- "EPSG:4326"
r1 <- r[[1]]

# Optional: if QGIS shows it upside-down, uncomment the next line
# r <- flip(r, direction = "vertical")

# Printing geometry summary
cat("\n--- Raster geometry ---\n")
print(r)
cat("\nExtent:\n"); print(ext(r1))
cat("\nResolution:\n"); print(res(r1))
cat("\nDimensions:\n"); print(dim(r1))
cat("\nCRS:\n"); print(crs(r1))

# Writing GeoTIFF file out
writeRaster(r1, outfile, overwrite = TRUE)
cat("\nSaved GeoTIFF to:", outfile, "\n")
