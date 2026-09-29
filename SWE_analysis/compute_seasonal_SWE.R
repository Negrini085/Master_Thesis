# The main goal of this script is to compute the seasonal SWE maps, in order to 
# make trend assessments. A snow season is the period between November and May (
# both months are included)
rm(list = ls())
gc()

years <- 1952:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Importing longitude and latitude
nc <- nc_open("Dataset/SC/SC_1951.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
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
  first_chunk <- ncvar_get(nc, "sc", start = c(1, 1, start), count = c(-1, -1, -1))
  nc_close(nc)
  
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  second_chunk <- ncvar_get(nc, "sc", start = c(1, 1, 1), count = c(-1, -1, end))
  nc_close(nc)
  
  stopifnot(identical(dim(first_chunk)[1:2], dim(second_chunk)[1:2]))
  swe <- abind(first_chunk, second_chunk, along = 3)
  
  
  # Creating snow cover duration map for a given hydrological year
  seasonal_swe[, , which(years == y)] <- rowMeans(swe, dims = 2, na.rm = FALSE)
  cat("Made seasonal map for year", y, "of our SWE record!", "\n")
}


# Saving SCD maps for the hydrological years covered by our SWE dataset
hy <- years

londim  <- ncdim_def("lon", "degrees_east",  as.double(lon))
latdim  <- ncdim_def("lat", "degrees_north", as.double(lat))
timedim <- ncdim_def("hydro_year", "year",   as.double(hy))

seasonal_swe_def <- ncvar_def(
  name        = "seasonal_swe",
  units       = "days",
  dim         = list(londim, latdim, timedim),
  missval     = -9999,
  longname    = "Seasonal average swe value (1 November - 31 May)",
  prec        = "float",
  compression = 5
)

ncout <- nc_create("Results/SWE/swe_seasonal_maps_1952_to_2023.nc", scd_def, force_v4 = TRUE)
ncvar_put(ncout, seasonal_swe_def, seasonal_swe)

ncatt_put(ncout, "lon", "axis", "X")
ncatt_put(ncout, "lat", "axis", "Y")
ncatt_put(ncout, "swe", "cell_methods", "hydro_year: mean")
ncatt_put(ncout, 0, "title", "Snow cover duration over hydrological years")
ncatt_put(ncout, 0, "source", "Derived from daily SC maps of the SWE dataset")
ncatt_put(ncout, 0, "Conventions", "CF-1.8")
ncatt_put(ncout, 0, "history", paste0(format(Sys.time(), "%Y-%m-%d"), ": created in R with ncdf4"))

nc_close(ncout)