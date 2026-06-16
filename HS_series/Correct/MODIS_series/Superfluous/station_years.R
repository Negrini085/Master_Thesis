# The main goal of this script is to evaluate when measures started and ended at 
# a specific station.
rm(list = ls())
gc()

fname_italian <- "Dataset/ITALIAN_STATIONS"
fname_station_data <- "../STATION_check/Dataset/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/MODIS_series/")

# Function to evaluate operating years. I will check first row content, as well as 
# whole year values to check whether aws station was actually active or not.
read_station <- function(fname) {
  
  # Opening data-frame and selecting values and years
  df <- read.table(fname, header = FALSE)
  years <- as.numeric(df$V1)

  return(c(min(years, na.rm = TRUE), max(years, na.rm = TRUE)))
}



# Reading station names and coordinates
df_ita <- read.table(fname_italian)
ita_station_names <- df_ita$V1

df_data <- read.table(fname_station_data, header = TRUE)
df_data <- data.frame(station_names = df_data$name, lon = df_data$lon, lat = df_data$lat, flag = df_data$flag)


# Selecting only stations we actually need to work with
mask <- df_data$station_names %in% ita_station_names
df_data <- df_data[mask, ]

start_year <- array(NA, dim = c(length(ita_station_names)))
end_year <- array(NA, dim = c(length(ita_station_names)))
station_names <- df_data[, 1]
flag <- df_data[, 4]


# Opening files and evaluating start/end years
for (name_ind in seq_len(length(station_names))) {
  
  fname <- paste0("../Dataset/", station_names[name_ind])
  
  appo <- tryCatch({
    read_station(fname)
  }, error = function(e) {
    message("Error: ", e$message)
    c(NA, NA)
  }, warning = function(w) {
    message("Warning: ", w$message)
    c(NA, NA)
  }, finally = {
    message("Evaluated measurement period for ", station_names[name_ind])
  })
  
  start_year[name_ind] <- appo[1] - 1
  end_year[name_ind]   <- appo[2]
}



# Building final dataframe and creating a file
df <- data.frame(
  names = ita_station_names, 
  lon = df_data$lon, 
  lat = df_data$lat, 
  start = start_year, 
  end = end_year, 
  flag = flag
)

write.table(df, file = "Dataset/start_end_years.dat", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")