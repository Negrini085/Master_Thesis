# The main goal of this script is to test station_year function. I will use some
# faulty files in the dataset to check function performance.
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
    if(logic_years[ind] == TRUE){break}
    ind <- ind + 1
  }
  start <- as.numeric(names(years)[ind]) + 1
  
  # Looking for the end of data series
  ind <- length(years)
  repeat{
    if(ind < 1) { end <- NA; break }
    if(logic_years[ind] == TRUE){break}
    ind <- ind - 1
  }
  end <- as.numeric(names(years)[ind])
  
  return(c(start, end))
}


# First test
appo <- read_station("Superfluous/Files/test1.dat", 2)
print(paste0("First test: ", appo[1], " to ", appo[2]))


# Second test
appo <- read_station("Superfluous/Files/test2.dat", 2)
print(paste0("Second test: ", appo[1], " to ", appo[2]))


# Third test
appo <- read_station("Superfluous/Files/test3.dat", 2)
print(paste0("Third test: ", appo[1], " to ", appo[2]))


# Fourth test
appo <- read_station("Superfluous/Files/test4.dat", 2)
print(paste0("Fourth test: ", appo[1], " to ", appo[2]))