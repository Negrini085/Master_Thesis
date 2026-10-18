# The main goal of this script is to evaluate the model predicted SWE evolution 
# over the overlap area with the Po Basin product.
rm(list = ls())
gc()

library(terra)
library(ncdf4)
library(ggplot2)

years <- 1991:2021
fname_model_map <- "../Dataset/SWE_1951.nc"
fname_dem <- "../../SWE_analysis/DEM/DEM_compatible.tif"
fname_po <- "../../SC_studies/Po-Basin/Dataset/1992/SWE_1991-10-03.tif"
fname_glacier_mask <- "../../SWE_analysis/Results/years_continuous_snow.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_comparison/with_POBasin/")


# Function to convert from tiff to netCDF format
raster_to_nc <- function(r, lon, lat) {
  stopifnot(length(lon) == ncol(r), length(lat) == nrow(r))
  xr  <- xFromCol(r)
  yr  <- yFromRow(r)
  tol <- min(res(r)) / 2
  ix <- sapply(lon, function(v) which.min(abs(xr - v)))
  iy <- sapply(lat, function(v) which.min(abs(yr - v)))
  if (max(abs(xr[ix] - lon)) > tol || max(abs(yr[iy] - lat)) > tol)
    stop("Grids are not overlapping")
  m <- as.matrix(r, wide = TRUE)
  t(m)[ix, iy, drop = FALSE]
}








# Importing model, Po Basin map and glacier area map to create common overlap region
glacier_mask <- rast(fname_glacier_mask)[[1]]
model_map <- rast(fname_model_map)[[1]]
glacier_mask <- glacier_mask > 36
model_map[glacier_mask] <- NA
po_map <- rast(fname_po)


# Projecting po map on model grid map
po_map <- project(po_map, model_map, method = "near")
compareGeom(po_map, model_map, stopOnError = FALSE)


# Masking procedure
overlap_mask <- !is.na(model_map) & !is.na(po_map)
area <- cellSize(po_map, unit = "m")


# Importing, projecting and cropping dem
dem <- rast(fname_dem)
compareGeom(dem, po_map, stopOnError = FALSE)
overlap_mask <- overlap_mask & !is.na(dem)
dem[!overlap_mask] <- NA
rm(po_map, model_map, glacier_mask); invisible(gc())


# Exporting maps to netcdf format. Importing map to make comparison of coordinates
nc <- nc_open(fname_model_map)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)

overlap_mask <- raster_to_nc(overlap_mask, lon, lat) == 1
elev <- raster_to_nc(dem, lon, lat)
area <- raster_to_nc(area, lon, lat)


















# Band elevation limits
breaks     <- c(-100, 1000, 2000, 5000)
n_bands    <- length(breaks) - 1
band_names <- paste0(breaks[-length(breaks)], "-", breaks[-1], "m")



# Cycle over years
swe_evo <- rep(list(numeric(0)), n_bands)
for(y in years){

  
  # Selecting SWE for a given year
  nc <- nc_open(paste0("../Dataset/SWE_", y, ".nc"))
  swe <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, -1))
  nc_close(nc)
  
  stopifnot(identical(dim(overlap_mask), dim(swe)[1:2]))
  stopifnot(identical(dim(elev), dim(swe)[1:2]))
  
  swe[!overlap_mask] <- NA
  swe_volume <- sweep(swe, c(1, 2), area, `*`)
  
  for(i in 1:n_bands){
    
    appo <- swe_volume
    mask_ele <- elev > breaks[i] & elev <= breaks[i+1] & !is.na(elev)
    
    appo[!mask_ele] <- NA
    swe_evo[[i]] <- c(swe_evo[[i]], colSums(appo, dims = 2, na.rm = TRUE)*10^-12)
  }
  
  cat("Taken into account year", y, "of our record!", "\n")
}


df <- data.frame(
  dates = seq(as.Date("1991-01-01"), as.Date("2021-12-31"), by = "day"), 
  lower = swe_evo[[1]],
  medium = swe_evo[[2]],
  higher = swe_evo[[3]],
  total = swe_evo[[1]] + swe_evo[[2]] + swe_evo[[3]]
)
write.table(df, file = "Results/model_swe_series.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)