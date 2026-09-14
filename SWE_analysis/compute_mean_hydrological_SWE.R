# The main goal of this script is to compute mean hydrological SWE maps, in order 
# to make some spatial trend assessment of the snowcover.
rm(list = ls())
gc()

library(abind)
library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Importing longitude and latitude
nc <- nc_open("Dataset/SWE/SWE_1951.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)

# Cycle over years
swe <- array(NA_real_, dim = c(length(lon), length(lat), (length(years)-1)))
for(y in years[2:length(years)]){
  
  # Selecting start and end days to convert to hydrological years
  start <- 274
  end <- 273
  if(y%%4 == 0) end <- 274
  if((y-1)%%4 == 0) start <- 275
  
  
  # Selecting snow cover for a given hydrological year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y-1, ".nc"))
  first_chunk <- ncvar_get(nc, "swe", start = c(1, 1, start), count = c(-1, -1, -1))
  nc_close(nc)
  
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  second_chunk <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, end))
  nc_close(nc)
  
  stopifnot(identical(dim(first_chunk)[1:2], dim(second_chunk)[1:2]))
  swe_hydro <- abind(first_chunk, second_chunk, along = 3)
  
  
  # Creating snow cover duration map for a given hydrological year
  swe[, , which(years == y) - 1] <- rowMeans(swe_hydro, dims = 2, na.rm = FALSE)
  cat("Made swe mean map for the hydrological year", y, "of our SWE record!", "\n")
}


# Saving SCD maps for the hydrological years covered by our SWE dataset
hy <- years[-1]

londim  <- ncdim_def("lon", "degrees_east",  as.double(lon))
latdim  <- ncdim_def("lat", "degrees_north", as.double(lat))
timedim <- ncdim_def("hydro_year", "year",   as.double(hy))

swe_def <- ncvar_def(
  name        = "swe",
  units       = "mm",
  dim         = list(londim, latdim, timedim),
  missval     = -9999,
  longname    = "Mean snow water equivalent (1 October - 30 September)",
  prec        = "float",
  compression = 5
)

ncout <- nc_create("Results/SWE/mean_swe_maps_hydro_1952_to_2023.nc", swe_def, force_v4 = TRUE)
ncvar_put(ncout, swe_def, swe)

ncatt_put(ncout, "lon", "axis", "X")
ncatt_put(ncout, "lat", "axis", "Y")
ncatt_put(ncout, "swe", "cell_methods", "hydro_year: mean")
ncatt_put(ncout, 0, "title", "Snow water equivalent over hydrological years")
ncatt_put(ncout, 0, "source", "Derived from daily SWE maps of the SWE dataset")
ncatt_put(ncout, 0, "Conventions", "CF-1.8")
ncatt_put(ncout, 0, "history", paste0(format(Sys.time(), "%Y-%m-%d"), ": created in R with ncdf4"))

nc_close(ncout)