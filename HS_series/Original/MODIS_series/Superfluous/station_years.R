# The main goal of this script is to evaluate when measures started and ended at 
# a specific station.
rm(list = ls())
gc()

fname_italian <- "Dataset/ITALIAN_STATIONS"
fname_station_data <- "../STATION_check/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Original/MODIS_series/")

# Function to evaluate operating years. I will check first row content, as well as 
# whole year values to check whether aws station was actually active or not.
read_station <- function(fname, nmonths) {
  
  # Opening data-frame and selecting values and years
  df <- read.table(fname, header = FALSE, sep = "", fill = TRUE)
  raw <- as.matrix(df[, -(1:2), drop = FALSE])
  years <- as.numeric(df[[1]])
  
  # Selecting operational years
  years <- table(years)
  # names(years) <- NULL
  
  # Evaluating which years contain actual datas. It's necessary because in Swiss
  # there are some stations whose datas are full of -90.
  logic_years <- array(TRUE, dim = c(length(years)))
  for(i in seq_len(length(years))){
    hs_series <- unlist(lapply(seq_len(nmonths), function(j) {
      x <- raw[j + (i-1)*nmonths, ]
      x <- x[!is.na(x)]
      x[x == -90] <- NA
      x
    }), use.names = FALSE)
    
    # Logical mask (checking if even a single data is here)
    mask <- !is.na(hs_series)
    appo <- length(hs_series[mask])
    
    if(appo == 0){
      logic_years[i] <- FALSE
    }
  }
  
  # Looking for the start of data series. Considering that here we are interested
  # in whole hydrological years, the actual start is one year grater than the one 
  # where datas start to appear
  ind <- 1
  repeat{
    if(ind > length(logic_years)) { start <- NA; break }
    if(logic_years[ind] == TRUE){
      start <- as.numeric(names(years)[ind])
      break
    }
    ind <- ind + 1
  }
  
  # Looking for the end of data series
  ind <- length(years)
  repeat{
    if(ind < 1) { end <- NA; break }
    if(logic_years[ind] == TRUE){
      end <- as.numeric(names(years)[ind])
      break
    }
    ind <- ind - 1
  }
  
  return(c(start, end))
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
rm(appo)
gc()



# Opening files and evaluating start/end years
for (name_ind in seq_len(length(station_names))) {
  
  fname <- paste0("../Dataset/", station_names[name_ind])
  
  appo <- tryCatch({
    read_station(fname, 12)
  }, error = function(e) {
    message("Error: ", e$message)
    c(NA, NA)
  }, warning = function(w) {
    message("Warning: ", w$message)
    c(NA, NA)
  }, finally = {
    message("Evaluated measurement period for ", station_names[name_ind])
  })
  
  start_year[name_ind] <- appo[1]
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