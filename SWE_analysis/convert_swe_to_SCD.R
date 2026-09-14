# The main goal of this script is to convert SWE maps into SCD maps, in order to 
# make snow cover duration analysis.
rm(list = ls())
gc()

library(ncdf4)

years <- 1951:2023
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")




# Importing longitude and latitude
nc <- nc_open("Dataset/SWE/SWE_1951.nc")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc)


# Cycle over years
area <- array(NA_real_, dim = c(length(lon), length(lat), (length(years)-1)))
for(y in years[2:length(years)]){
  
  # Selecting start and end days to convert to hydrological years
  start <- 274
  end <- 273
  if(y%%4 == 0) end <- 274
  if((y-1)%%4 == 0) start <- 275
  
  # Selecting SWE for a given year
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y-1, ".nc"))
  first_chunk <- ncvar_get(nc, "swe", start = c(1, 1, start), count = c(-1, -1, -1))
  nc_close(nc)
  
  nc <- nc_open(paste0("Dataset/SWE/SWE_", y-1, ".nc"))
  second_chunk <- ncvar_get(nc, "swe", start = c(1, 1, 1), count = c(-1, -1, end))
  nc_close(nc)
  
  cat("Taken into account year", y, "of our record!", "\n")
}



# Saving data
df <- data.frame(data = seq(as.Date("1951-10-01"), by = "day", length.out = length(swe_evo)), swe = swe_evo)
write.table(df, file = "swe_evolution.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)