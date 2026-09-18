# The main goal of this script is to assess SCD trends in order to finish the MODIS 
# section of my thesis
rm(list = ls())
gc()

library(trend)
library(terra)

hydro_years <- 2001:2025
fname_mask <- "Dataset/annual_maps/LOS/los_2025.tif"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/MODIS/")


# Function to assess Mann-Kendall test
mk_fun <- function(v) {
  if (any(is.na(v))) return(c(NA_real_, NA_real_))
  # if (sum(v != 0, na.rm = TRUE) < 10){
  #   s <- trend::sens.slope(v)
  #   return(c(unname(s$estimates), 1))
  # }
  s <- trend::sens.slope(v)
  c(unname(s$estimates), s$p.value)
}


for(y in hydro_years){

  # Selecting correct files to import
  # dates_first <- seq(as.Date(paste0(y-1, "-10-01")), as.Date(paste0(y-1, "-12-31")), "day")
  # fnames_first <- paste0("Dataset/appo/", y-1, "/day_", dates_first, ".tif")

  dates_second <- seq(as.Date(paste0(y, "-01-25")), as.Date(paste0(y, "-09-30")), "day")
  fnames_second <- paste0("Dataset/appo/", y, "/day_", dates_second, ".tif")


  # Importing rasters
  fnames <- fnames_second
  sc_hydro_maps <- rast(fnames)
  sc_hydro_maps <- subst(sc_hydro_maps, 2, NA)


  # Computing snow cover duration map
  scd <- 116 + sum(sc_hydro_maps, na.rm = TRUE)
  names(scd) <- paste0("SED_", y)
  writeRaster(scd, file.path("Dataset/appo/SED", paste0("SED_", y, ".tif")), overwrite = TRUE)
  cat("Made SED computations for ", y, "\n")
}


# Importing snow cover duration stack and Italian mask in order to focus only on days I'm interested in
scd_stack <- rast(list.files("Dataset/appo/SED", pattern = "^SED_.*\\.tif$", full.names = TRUE))
scd_mask <- rast(fname_mask)


# Masking pixels outside Italian domain
compareGeom(scd_stack, scd_mask)
scd_stack <- mask(scd_stack, is.na(scd_mask), maskvalue = TRUE)


# Making trend evaluation using non parametric tests
trend_maps <- app(scd_stack, mk_fun)
names(trend_maps) <- c("slope", "pval")
writeRaster(
  trend_maps, "Dataset/appo/SED/trend_maps.tif", overwrite = TRUE,
  datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES")
  )