# The main goal of this script is to filter stations based on start and end date
# for measures. I want to take care of NAs and time windows that not intersect 
# with MODIS one.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")


# Loading Italian stations whose elevation is comparable with the 30-meter DEM
fname <- "Datas/start_end_years.dat"
appo <- as.matrix(read.table(fname, header = FALSE))

station_names <- appo[, 1]
station_lon <- as.numeric(appo[, 2])
station_lat <- as.numeric(appo[, 3])
station_end <- as.numeric(appo[, 5])
station_start <- as.numeric(appo[, 4])
rm(appo)
gc()


# First check -> both NAs
mask <- is.na(station_start) & is.na(station_end)
print("Stations with both start and end year NAs: ")
for(name in station_names[mask]){
  print(name)
}

station_lon <- station_lon[!mask]
station_lat <- station_lat[!mask]
station_end <- station_end[!mask]
station_names <- station_names[!mask]
station_start <- station_start[!mask]


# Second check -> one NA
mask <- is.na(station_start) | is.na(station_end)
print("Stations with one NA: ")
if(sum(mask) == 0){
  print("None")
} else{
  for(name in station_names[mask]){
    print(name)
  }
}

station_lon <- station_lon[!mask]
station_lat <- station_lat[!mask]
station_end <- station_end[!mask]
station_names <- station_names[!mask]
station_start <- station_start[!mask]


# Third check -> compatibility with MODIS time-window
mask <- station_end < 2000
print("Stations with time-window non-superimpoasble with MODIS one: ")
if(sum(mask) == 0){
  print("None")
} else{
  for(name in station_names[mask]){
    print(name)
  }
}

station_lon <- station_lon[!mask]
station_lat <- station_lat[!mask]
station_end <- station_end[!mask]
station_names <- station_names[!mask]
station_start <- station_start[!mask]


# Printing filtered dataset
df <- data.frame(
  names = station_names, 
  lon = station_lon, 
  lat = station_lat, 
  start = station_start, 
  end = station_end
)

write.table(df, file = "Datas/start_end_years_filtered.dat", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")