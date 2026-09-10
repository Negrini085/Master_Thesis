# The main goal of this script is to convert PCPD series in PCPD rasters, in order to assess model 
# performance.
rm(list = ls())
gc()

library(ncdf4)

fillval <- -9999
years <- 1951:2023
output_dir <- "../../Dataset/PCPD/"
input_dir <- "../../../SWE_calibration/Dataset/PCPD/"
fname_anagrafica <- "../../../HS_series/Original/STATION_check/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Model/")



# File names for SWE series and metadata file
files <- list.files(path = input_dir, full.names = TRUE)
anag <- read.table(fname_anagrafica, header = TRUE)
ids <- basename(files)

if (length(files) == 0) stop("No files in: ", input_dir)

key <- sub("DV_", "", ids)
idx <- match(key, anag$name)
if (anyNA(idx)) stop("No metadata for: ", paste(ids[is.na(idx)], collapse = ", "))
anag <- anag[idx, ]




# Creating a matrix containing all SWE series
dates <- seq(as.Date("1951-01-01"), as.Date("2023-12-31"), by = "day")
pcpd <- matrix(NA_real_, nrow = length(dates), ncol = length(files))

for (i in seq_along(files)) {
  d  <- read.table(files[i], col.names = c("y", "m", "d", "seq", "prec"))
  dt <- as.Date(sprintf("%04d-%02d-%02d", d$y, d$m, d$d))
  
  pcpd[match(dt, dates), i] <- d$prec
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
  v_pcpd  <- ncvar_def(
    "prec", "mm", list(dim_stat, dim_time), fillval, longname = "total precipitation", 
    prec = "double", compression = 5
  )
  
  nc <- nc_create(
    file.path(output_dir, sprintf("PCPD_%d.nc", yr)), 
    list(v_name, v_lon, v_lat, v_alt, v_pcpd), force_v4 = TRUE
  )
  
  ncvar_put(nc, v_name, ids)
  ncvar_put(nc, v_lon,  anag$lon)
  ncvar_put(nc, v_lat,  anag$lat)
  ncvar_put(nc, v_alt,  anag$ele)
  ncvar_put(nc, v_pcpd,  t(pcpd[mask, ]))
  
  ncatt_put(nc, "station_name", "cf_role", "timeseries_id")
  ncatt_put(nc, "lon", "standard_name", "longitude")
  ncatt_put(nc, "lat", "standard_name", "latitude")
  ncatt_put(nc, "alt", "standard_name", "height_above_mean_sea_level")
  ncatt_put(nc, "alt", "positive", "up")
  ncatt_put(nc, "prec", "coordinates", "lat lon alt station_name")
  ncatt_put(nc, 0, "featureType", "timeSeries")
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "title", paste("Daily precipitation -", yr))
  
  nc_close(nc)
  cat("Created PCPD_", yr, ".nc\n", sep = "")
}
