# The main goal of this script is to inspect input netcdf files content in order 
# to later asses if some datas are faulty
rm(list = ls())
gc()

library(ncdf4)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_datas/Input/PCPD/")
f_name <- "../../../Input/PCPD/1951.nc"

# Opening netCDF file of SWE
nc <- nc_open(f_name)
cat("\n\nDataset loaded successfully!\n\n")
print(nc)

# names() is a function that state variable names
cat("\n\n\nVariable names are: \n")
cat(names(nc$var))
cat("\n\nDimension names are: \n")
cat(names(nc$dim))
cat("\n")

# It's useful to note that you can get datas simply by using ncvar_get(). Time is an
# integer number that starts from the first of january 1950.
time <- ncvar_get(nc, "time")
dates <- as.Date("1950-01-01") + time

target_date <- as.Date("1951-01-15")
time_index <- which(dates == target_date)
precip <- ncvar_get(nc,"total_precipitation", start = c(1, 1, time_index), count = c(-1, -1, 1))