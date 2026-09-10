# The main goal of this script is to convert SWE series in SWE rasters, in order to assess model performance.
rm(list = ls())
gc()

library(ncdf4)

fillval <- -9999
years <- 1951:2023
output_dir <- "Dataset/Sequential/"
input_dir <- "../../../SWE_calibration/Results/raw/"
fname_anagrafica <- "../../../HS_series/Original/STATION_check/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Model/")



# File names for SWE series and metadata file
files <- list.files(path = input_dir, full.names = TRUE)
anag <- read.table(fname_anagrafica, header = TRUE)
ids <- basename(files)

if (length(files) == 0) stop("No files in: ", input_dir)

key <- sub("V_SDH", "HSD", ids)
idx <- match(key, anag$name)
if (anyNA(idx)) stop("No metadata for: ", paste(ids[is.na(idx)], collapse = ", "))
anag <- anag[idx, ]




# Creating a matrix containing all SWE series
dates <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
swe <- matrix(NA_real_, nrow = length(dates), ncol = length(files))

for (i in seq_along(files)) {
  d  <- read.table(files[i], col.names = c("y", "m", "d", "swe"))
  dt <- as.Date(sprintf("%04d-%02d-%02d", d$y, d$m, d$d))
  
  swe[match(dt, dates), i] <- d$swe
  if (i %% 100 == 0) cat("Taken into account ", i, "/", length(files), " series\n")
}


# Ready to create a netCDF per year
yy <- as.integer(format(dates, "%Y"))
maxchar  <- max(nchar(ids))


# Cycle over years in order to create a netCDF per year
for (yr in years) {
  
  mask <- which(yy == yr)
  origin  <- as.Date(paste0(yr, "-01-01"))
  
  dim_stat <- ncdim_def("station", "", 1:length(files), create_dimvar = FALSE)
  dim_char <- ncdim_def("name_strlen", "", 1:maxchar, create_dimvar = FALSE)
  dim_time <- ncdim_def(
    "time", paste0("days since ", yr, "-01-01 00:00:00"), as.numeric(dates[mask]-origin), 
    unlim = TRUE, calendar = "standard"
    )
  
  v_name <- ncvar_def("station_name", "", list(dim_char, dim_stat), prec = "char")
  v_lon  <- ncvar_def("lon", "degrees_east",  dim_stat, fillval, prec = "double")
  v_lat  <- ncvar_def("lat", "degrees_north", dim_stat, fillval, prec = "double")
  v_alt  <- ncvar_def("alt", "m",             dim_stat, fillval, prec = "double")
  v_swe  <- ncvar_def(
    "swe", "mm", list(dim_stat, dim_time), fillval, longname = "snow water equivalent", 
    prec = "float", compression = 5
    )
  
  nc <- nc_create(
    file.path(output_dir, sprintf("SWE_%d.nc", yr)), 
    list(v_name, v_lon, v_lat, v_alt, v_swe), force_v4 = TRUE
  )
  
  ncvar_put(nc, v_name, ids)
  ncvar_put(nc, v_lon,  anag$lon)
  ncvar_put(nc, v_lat,  anag$lat)
  ncvar_put(nc, v_alt,  anag$ele)
  ncvar_put(nc, v_swe,  t(swe[mask, ]))
  
  ncatt_put(nc, "station_name", "cf_role", "timeseries_id")
  ncatt_put(nc, "lon", "standard_name", "longitude")
  ncatt_put(nc, "lat", "standard_name", "latitude")
  ncatt_put(nc, "alt", "standard_name", "height_above_mean_sea_level")
  ncatt_put(nc, "alt", "positive", "up")
  ncatt_put(nc, "swe", "coordinates", "lat lon alt station_name")
  ncatt_put(nc, 0, "featureType", "timeSeries")
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "title", paste("Daily SWE -", yr))
  
  nc_close(nc)
  cat("Created SWE_", yr, ".nc\n", sep = "")
}
