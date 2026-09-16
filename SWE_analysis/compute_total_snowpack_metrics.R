# The main goal of this script is to compute the total volume of swe which is stored 
# within our mountains
rm(list = ls())
gc()

library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

# Function to create an area map in order to later retrive the water volume stored 
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




# Computing area
area <- make_area_map("Dataset/SWE/SWE_1951.nc")

# Cycle over years
swe_evo <- numeric(0)
mean_height <- numeric(0)
covered_pixels <- numeric(0)
data <- as.Date(character(0))
mean_over_covered <- numeric(0)
for(y in years){
  
  if(y%%4 == 0) next
  
  # Selecting start and end days to consider for a given year
  start <- 1
  end <- -1
  if(y == 1951) start <- 274
  if(y == 2023) end <- 273
  
  # Selecting SWE for a given year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y, ".nc"))
  swe <- ncvar_get(nc, "swe", start = c(1, 1, start), count = c(-1, -1, end))
  nc_close(nc)
  
  if(end == -1) data <- c(data, seq((as.Date(paste0(y-1, "-12-31"))+start), as.Date(paste0(y, "-12-31")),"day"))
  else data <- c(data, seq((as.Date(paste0(y-1, "-12-31"))+start), as.Date(paste0(y, "-09-30")),"day"))
  
  appo <- sweep(swe, c(1, 2), area, `*`)
  annual_swe_evo <- colSums(appo, dims = 2, na.rm = TRUE)*10^-12
  annual_mean_height <- colSums(swe, dims = 2, na.rm = TRUE)/sum(!is.na(swe[, , 1]), na.rm = TRUE)
  annual_mean_over_covered <- colSums(swe, dims = 2, na.rm = TRUE)/colSums(swe > 0, dims = 2, na.rm = TRUE)
  
  
  appo <- swe
  appo <- swe > 0
  annual_covered_pixels <- colSums(appo, dims = 2, na.rm = TRUE)
  
  
  swe_evo <- c(swe_evo, annual_swe_evo)
  mean_height <- c(mean_height, annual_mean_height)
  covered_pixels <- c(covered_pixels, annual_covered_pixels)
  mean_over_covered <- c(mean_over_covered, annual_mean_over_covered)
  cat("Taken into account year", y, "of our record!", "\n")
}



# Saving data
df <- data.frame(
  data = data, 
  swe = swe_evo,
  mean_swe = mean_height,
  covered = covered_pixels,
  mean_over_covered = mean_over_covered
  )
write.table(df, file = "Results/SWE/total_snowpack_metrics.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)