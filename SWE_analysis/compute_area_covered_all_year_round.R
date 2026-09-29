# The main goal of this script is to compute the total area, on an hydrological year standpoint, that is completely snow covered all year round.
rm(list = ls())
gc()

library(ncdf4)

years <- 1952:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Function to create an area map in order to later retrieve the water volume stored 
# as swe * area (and then summing over the whole investigated domain)
make_area_map <- function(fname_template){
  
  # Opening netCDF in orded to retrive longitude and latitude informations
  nc <- nc_open(fname_template)
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  nc_close(nc)
  
  # Checking latitude and longitude steps
  dlat <- abs(lat[45] - lat[44])
  dlon <- abs(lon[45] - lon[44])
  stopifnot(dlat == dlon)
  
  
  # Computing pixel areas
  rEarth <-  6371005.0
  colat <- (90 - lat)*2*pi/360          # Evaluating colatitude as radiant variable
  sp <- (lat[45] - lat[44])*2*pi/360    # We have to do degree -> radiant conversion
  area <- array(rEarth^2 * sp^2, dim = c(length(lon), length(lat)))
  
  # Final pixel area computation
  for(i in 1:length(lat)){
    area[, i] <- sin(colat[i]) * area[, i]
  }
  
  return(area)
}










area <- make_area_map("Dataset/SWE/SWE_1952.nc")
covered_area <- numeric(0)

# Cycle over years
for(y in years){
  # Selecting SWE for a given year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  appo <- ncvar_get(nc, "swe")
  len_year <- dim(appo)[3]
  nc_close(nc)
  swe <- appo
  
  
  # Masking pixels to map snow coverage
  mask <- appo > 10 & !is.na(appo)
  swe[mask] <- 1
  
  mask <- appo <= 10 & !is.na(appo)
  swe[mask] <- 0
  
  
  # Selecting pixels which are snow covered the whole year
  snow_season <- rowSums(swe, dims = 2, na.rm = FALSE)
  rm(appo, swe, mask); invisible(gc())
  
  mask <- snow_season == len_year
  if(any(dim(mask) != dim(area))) stop("No compatible dimensions between area and cover map!")
  
  appo_area <- sum(area[mask], na.rm = TRUE)*10^-6
  covered_area <- c(covered_area, appo_area)
  cat("Taken care of", y, "!\n")
}



df_print <- data.frame(
  year = years,
  area = covered_area
)

write.table(df_print, "Results/csc_covered_area.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)