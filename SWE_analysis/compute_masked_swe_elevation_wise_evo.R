# The main goal of this script is to compute masked SWE evolution elevation wise.
rm(list = ls())
gc()

library(terra)
library(ncdf4)

years <- 1951:2023
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










# Importing glaciated area mask and computing area maps
nc <- nc_open("Results/years_continuous_snow.nc")
years_csc <- ncvar_get(nc, "years_continuous_snow")
mask_csc <- !is.na(years_csc) & years_csc >= 36
area <- make_area_map("Dataset/SWE/SWE_1951.nc")
nc_close(nc)


# Checking correct orientation of netCDF file with DEM elevations
nc <- nc_open("Dataset/SWE/SWE_1951.nc")
lon_nc <- as.vector(ncvar_get(nc, "lon"))
lat_nc <- as.vector(ncvar_get(nc, "lat"))
nc_close(nc)

dem  <- rast("DEM/DEM_compatible.tif")[[1]]
elev <- t(as.matrix(dem, wide = TRUE))
lat_dem <- yFromRow(dem, 1:nrow(dem))
lon_dem <- xFromCol(dem, 1:ncol(dem))
if (lat_nc[1] < lat_nc[length(lat_nc)]) {
  elev    <- elev[, ncol(elev):1]
  lat_dem <- rev(lat_dem)
}
stopifnot(identical(dim(elev), dim(mask_csc)), max(abs(lon_dem - lon_nc)) < 1e-4, max(abs(lat_dem - lat_nc)) < 1e-4)


# Band elevation limits
breaks     <- c(-100, 500, 1000, 1500, 2000, 2500, 5000)
n_bands    <- length(breaks) - 1
band_names <- paste0(breaks[-length(breaks)], "-", breaks[-1], "m")



# Cycle over years
swe_evo <- rep(list(numeric(0)), 6)
for(y in years){
  
  # Selecting start and end days to consider for a given year
  start <- 1
  end <- -1
  if(y == 1951) start <- 274
  if(y == 2023) end <- 273
  
  # Selecting SWE for a given year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  swe <- ncvar_get(nc, "swe", start = c(1, 1, start), count = c(-1, -1, end))
  nc_close(nc)
  
  stopifnot(identical(dim(mask_csc), dim(swe)[1:2]))
  swe[mask_csc] <- NA
  
  swe_volume <- sweep(swe, c(1, 2), area, `*`)
  for(i in 1:n_bands){
    
    appo <- swe_volume
    mask_ele <- elev > breaks[i] & elev <= breaks[i+1] & !is.na(elev)
    
    appo[!mask_ele] <- NA
    swe_evo[[i]] <- c(swe_evo[[i]], colSums(appo, dims = 2, na.rm = TRUE)*10^-12)
  }

  cat("Taken into account year", y, "of our record!", "\n")
}


df <- data.frame(
  data = seq(as.Date("1951-10-01"), as.Date("2023-09-30"), by = "day"), 
  first = swe_evo[[1]],
  second = swe_evo[[2]],
  third = swe_evo[[3]],
  fourth = swe_evo[[4]],
  fifth = swe_evo[[5]],
  sixth = swe_evo[[6]],
  total = swe_evo[[1]] + swe_evo[[2]] + swe_evo[[3]] + swe_evo[[4]] + swe_evo[[5]] + swe_evo[[6]]
  )
write.table(df, file = "Results/SWE/masked_swe_elevation_wise_evolution.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)