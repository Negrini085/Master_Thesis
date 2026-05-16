# The main goal of this script is to compute DDF for every station in order to later
# be able to maek plots
rm(list = ls())
gc()

fname <- "Results/ddf.dat"
fname_ana <- "../../Ours/STATION_check/Correcting/ANAGRAFICA_CORRECT"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Importing DDF for every hydrological year
df <- read.table(fname, header = TRUE)
ddf <- as.numeric(df$ddf)
station_names <- df$name


# Importing ANAGRAFICA
df <- read.table(fname_ana, header = TRUE)
station_names_ana <- df$station_name
ele <- as.numeric(df$ele_rev)
lon <- as.numeric(df$lon_rev)
lat <- as.numeric(df$lat_rev)


# Cycle across stations
appo_ddf <- numeric(0)
appo_ele <- numeric(0)
appo_lon <- numeric(0)
appo_lat <- numeric(0)
appo_name <- character(0)
for(name in unique(station_names)){
  
  # Selecting DDF for a given station
  mask <- station_names == name
  station_ddf <- median(ddf[mask], na.rm = TRUE)
  if(length(ddf[mask]) < 10) next
  
  # Selecting coordinates & elevation for a given station
  mask <- station_names_ana == name
  station_ele <- ele[mask]
  station_lon <- lon[mask]
  station_lat <- lat[mask]
  
  # Storing values
  appo_lon <- c(appo_lon, station_lon)
  appo_lat <- c(appo_lat, station_lat)
  appo_ele <- c(appo_ele, station_ele)
  appo_ddf <- c(appo_ddf, station_ddf)
  appo_name <- c(appo_name, name)
}



# Saving values to file
df_print <- data.frame(
  name = appo_name, 
  lon = appo_lon, 
  lat = appo_lat,
  ele = appo_ele, 
  ddf = appo_ddf
)

write.table(df_print, "Results/station_ddf.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)