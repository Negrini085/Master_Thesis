# The main goal of this script is to make SCD trend estimates, to evaluate if the 
# snow season have shortened or not during the study period.
rm(list = ls())
gc()

library(trend)
library(ncdf4)

fname <- "Results/SCD/scd_maps_hydro_1952_to_2023_lim_15.nc"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Function to evaluate pixel by pixel trends
trend_pixel <- function(x) {
  if (any(is.na(x))) return(c(NA_real_, NA_real_))
  res <- tryCatch(sens.slope(x), error = function(e) NULL)
  if (is.null(res)) return(c(NA_real_, NA_real_))
  c(unname(res$estimates), res$p.value)
}


# Importing annual scd maps
nc <- nc_open(fname)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
scd_maps <- ncvar_get(nc, "scd")
n_years <- dim(scd_maps)[3]
nc_close(nc)



out <- apply(scd_maps, c(1, 2), trend_pixel)
slope_map <- out[1, , ]
pval_map  <- out[2, , ]

pval_fdr <- matrix(p.adjust(pval_map, method = "BH"), nrow = nrow(pval_map))
slope_decade <- 10*slope_map



dim_lon <- ncdim_def("lon", "degrees_east", lon)
dim_lat <- ncdim_def("lat", "degrees_north", lat)

v_slope <- ncvar_def("sen_slope", "days/decade", list(dim_lon, dim_lat), NA)
v_pval  <- ncvar_def("p_value", "-", list(dim_lon, dim_lat), NA)
v_pfdr  <- ncvar_def("p_value_fdr", "-", list(dim_lon, dim_lat), NA)

nc_out <- nc_create("Results/SCD/scd_trend_1952_2023.nc", list(v_slope, v_pval, v_pfdr))
ncvar_put(nc_out, v_slope, slope_decade)
ncvar_put(nc_out, v_pval, pval_map)
ncvar_put(nc_out, v_pfdr, pval_fdr)
nc_close(nc_out)