# The main goal of this script is to compute seasonal climatologies, in order to 
# show different precipitation patterns during different seasons.
rm(list = ls())
gc()

library(ncdf4)

years <- 1991:2020
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/PCPD/")



# Cycle over years to compute seasonal climatologies
annual_prec <- NULL
for(y in years){
  
  # Importing netCDF file
  fname <- paste0("../../Dataset/PCPD/", y, ".nc")
  nc <- nc_open(fname)
  lon <- ncvar_get(nc,"lon")
  lat <- ncvar_get(nc,"lat")
  prec <- ncvar_get(nc,"total_precipitation", start = c(1, 1, 244), count = c(-1, -1, 92))
  nc_close(nc)
  
  
  # Updating annual climatology
  if (is.null(annual_prec)) {
    annual_prec <- rowSums(prec, dims = 2, na.rm = FALSE)/length(years)
  } else {
    annual_prec <- annual_prec + rowSums(prec, dims = 2, na.rm = FALSE)/length(years)
  }
  print(paste0("Correctly added ", y))
}



# Saving seasonal climatologies
dim_lon <- ncdim_def("lon", "degrees_east", lon)
dim_lat <- ncdim_def("lat", "degrees_north", lat)
var_p <- ncvar_def(
  "autumn_precipitation", "mm", list(dim_lon, dim_lat),
  missval = -9999, longname = "Mean autumn precipitation 1991-2020"
)

nc_out <- nc_create("Results/autumn_prec_clim_1991_2020.nc", var_p)
ncvar_put(nc_out, var_p, annual_prec)
nc_close(nc_out)