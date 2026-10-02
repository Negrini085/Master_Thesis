# The main goal of this script is to compute the seasonal SWE maps, in order to 
# make trend assessments. A snow season is the period between November and May (
# both months are included)
rm(list = ls())
gc()

library(ncdf4)
library(abind)

years <- 1952:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Importing longitude and latitude
nc <- nc_open("Dataset/SWE/SWE_1951.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
units_att <- ncatt_get(nc, "swe", "units")
swe_units <- if (units_att$hasatt) units_att$value else "mm"
nc_close(nc)

# Cycle over years
seasonal_swe <- array(NA_real_, dim = c(length(lon), length(lat), length(years)))
for(y in years){
  
  # Selecting start and end days to convert to hydrological years
  start <- 305
  end <- 151
  if(y%%4 == 0) end <- 152
  if((y-1)%%4 == 0) start <- 306
  
  
  # Selecting snow cover for a given hydrological year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y-1, ".nc"))
  first_chunk <- ncvar_get(nc, "swe", start = c(1, 1, start), count = c(-1, -1, -1))
  nc_close(nc)
  
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  second_chunk <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, end))
  nc_close(nc)
  
  stopifnot(identical(dim(first_chunk)[1:2], dim(second_chunk)[1:2]))
  swe <- abind(first_chunk, second_chunk, along = 3)
  
  
  # Creating snow cover duration map for a given hydrological year
  seasonal_swe[, , which(years == y)] <- rowMeans(swe, dims = 2, na.rm = FALSE)
  cat("Made seasonal map for year", y, "of our SWE record!", "\n")
}





# Saving seasonal SWE
t_units <- "days since 1950-01-01"
origin  <- as.Date("1950-01-01")
t_start <- as.numeric(as.Date(paste0(years - 1, "-11-01")) - origin)
t_end   <- as.numeric(as.Date(paste0(years, "-06-01")) - origin)
t_mid   <- (t_start + t_end) / 2

dim_lon  <- ncdim_def("lon", "degrees_east", as.double(lon), longname = "longitude")
dim_lat  <- ncdim_def("lat", "degrees_north", as.double(lat), longname = "latitude")
dim_time <- ncdim_def("time", t_units, t_mid, calendar = "standard", longname = "time")
dim_nv   <- ncdim_def("nv", "", 1:2, create_dimvar = FALSE)

var_swe <- ncvar_def("swe", swe_units, list(dim_lon, dim_lat, dim_time),
                     missval = -9999, prec = "float", compression = 4,
                     longname = "Mean snow water equivalent over the November-May season")
var_bnds <- ncvar_def("time_bnds", t_units, list(dim_nv, dim_time), prec = "double")

out_file <- "Results/SWE/seasonal_mean_SWE_1952_2023.nc"
nc_out <- nc_create(out_file, list(var_swe, var_bnds), force_v4 = TRUE)

ncvar_put(nc_out, var_swe, seasonal_swe)
ncvar_put(nc_out, var_bnds, rbind(t_start, t_end))

ncatt_put(nc_out, "lon",  "axis", "X")
ncatt_put(nc_out, "lat",  "axis", "Y")
ncatt_put(nc_out, "time", "axis", "T")
ncatt_put(nc_out, "time", "bounds", "time_bnds")
ncatt_put(nc_out, "swe",  "cell_methods", "time: mean")
ncatt_put(nc_out, 0, "Conventions", "CF-1.8")
ncatt_put(nc_out, 0, "title", "Seasonal (November-May) mean SWE, seasons 1951/52-2022/23")
ncatt_put(nc_out, 0, "comment", "Season y spans 1 November y-1 to 31 May y")
ncatt_put(nc_out, 0, "history", paste(format(Sys.time()), "created with R ncdf4"))
nc_close(nc_out)

cat("Saved", out_file, "\n")