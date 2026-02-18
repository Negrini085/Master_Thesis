# The main goal of this script is to inspect the content of a station HSD series,
# just to get used to the format and the content. What we want to do here is to
# start developing procedures to distinguish between well-behaved datas and faulty ones.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")
fname <- "Dataset/ok_stations_dem_30.dat"


# Function to compute number of lines in a file.
num_lines <- function(fname){
  
  # Opening file connection
  num_lines <- 0
  con <- file(fname, open="r") 

  repeat{
    
    # Reading line and getting ready to close file connection if the first one
    # is already a numeric(0), which is the value returned when no item is found
    appo <- readLines(con, n = 1L)
    if(length(appo) == 0) break
    
    # If we got here, it means that the line wasn't void. I will add one to number
    # of lines
    num_lines <- num_lines + 1
  }
  
  # Closing file connection and returning num_lines content
  close(con)
  return(num_lines)
}



# Funciton to read the whole content of a station file. I think that it's not the
# best procedure, could prove faulty under multiple circumstances, but that's what
# I will use right now to just plot HS series and stuff like that.
read_station <- function(fname, item_sep){
  
  # Opening file connection
  appo <- character(0)
  hs_series <- numeric(0)
  con <- file(fname, open="r") 
  
  repeat{
    
    # Reading line and getting ready to close file connection if the first one
    # is already a numeric(0), which is the value returned when no item is found
    appo <- readLines(con, n = 1L)
    if(length(appo) == 0) break
    
    # If we got here, it means that the line wasn't void. We now need to split it
    # based on item separation. We also want to discard the first two items of 
    # every line, because those are year and month
    parts <- if (grepl(item_sep, appo, fixed = TRUE)) strsplit(appo, item_sep, fixed = TRUE)[[1]] else c(appo)
    if(length(parts) < 30 || length(parts) > 33){
      print(length(hs_series))
      print(paste0("Monthly snow height not loaded correctly! Check please ", fname))
      return(numeric(0))
    }
    to_keep <- as.numeric(parts[3:length(parts)])
    to_keep[to_keep == -90] <- NA_real_
    
    # Coupling together string lines
    hs_series <- c(hs_series, to_keep)
  }
  
  # Closing file connection and returning num_lines content
  close(con)
  return(hs_series)
}



# Importing stations whose elevation is comparable to the one of a DEM with
# resolution of 30 meters. We will select the name of one station and then we
# will discard the rest.
appo <- read.table(fname, header = TRUE, fill = TRUE)
station_name <- appo[[1]][148]

print(paste0("The following analysis will be on ", station_name, " which is a station"))
print(paste0("sitting at ", appo[[4]][148], " m asl.   Coordinates: ", appo[[2]][148], " E, ", appo[[3]][148], " N"))

rm(appo)
gc()



# Creating filename and evaluating number of lines in a single file
fname <- paste0("Dataset/HSD_", station_name)
appo <- num_lines(fname)

print(paste0("The file we are dealing with has ", appo, " lines!"))
print(paste0("This means that it contains datas for ", appo/12, " years!"))

rm(appo)
gc()


# Trying to read whole file content
item_sep <- "  "
hs_piamprato <- read_station(fname = fname, item_sep = item_sep)