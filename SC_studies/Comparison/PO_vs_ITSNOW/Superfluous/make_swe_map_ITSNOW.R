# The main goal of this script is create mean SWE maps over November - June period for ITSNOW
rm(list = ls())
gc()

library(ncdf4)

fname <- "../../IT-Snow/Datas/swe_seasonal_maps.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/Comparison/PO_vs_ITSNOW/")


# Getting swe maps
nc <- nc_open(fname)
lon <- ncvar_get(nc, names(nc$dim)[1])
lat <- ncvar_get(nc, names(nc$dim)[2])
swe <- ncvar_get(nc, names(nc$var)[1], start = c(1, 1, 1), count = c(-1, -1, 11))
nc_close(nc)


# Taking mean across third dimension
mean_map <- rowMeans(swe, dims = 2)

mask <- mean_map == 0
mean_map[mask] <- NA

# Saving values on tif file
r <- rast(t(mean_map)[nrow(t(mean_map)):1, ],
          extent = ext(min(lon), max(lon), min(lat), max(lat)),
          crs = "EPSG:4326")
writeRaster(r, "Dataset/mean_SWE_ITSNOW.tif", overwrite = TRUE,
            datatype = "FLT4S", NAflag = -9999,
            gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))