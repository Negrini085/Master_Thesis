# The main goal of this script is to evaluate when measures started and ended at 
# a specific station.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/MODIS_series/")

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
appo <- as.matrix(read.table("Datas/not_compatible.dat", header = FALSE))
coord_ele <- matrix(as.numeric(appo[, 2:3]), ncol = 2)
start_year <- array(NA, dim = c(length(coord_ele[, 1])))
end_year <- array(NA, dim = c(length(coord_ele[, 1])))
station_names <- appo[, 1]
rm(appo)
gc()



# Opening files and evaluating start/end years
for(name_ind in seq_len(length(station_names))){
  appo <- numeric(0)
  fname <- paste0("../Dataset/", station_names[name_ind])
  
  tryCatch({
    
    appo <- read_station(fname, 12)
    end_year[name_ind] <- appo[2]
    start_year[name_ind] <- appo[1]
    
  }, error = function(e) {
    
    end_year[name_ind] <- NA
    start_year[name_ind] <- NA
    message("Error: ", e$message)
    
  }, warning = function(w) {
    message("Warning: ", w$message)
    
  }, finally = {
    message(paste0("Evaluated measurament period for ", station_names[name_ind]))
  })
}



# Building final dataframe and creating a file
df <- data.frame(
  names = station_names, 
  lon = coord_ele[, 1], 
  lat = coord_ele[, 2], 
  start = start_year, 
  end = end_year
)

write.table(df, file = "Datas/start_end_years_non_compatible.dat", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")