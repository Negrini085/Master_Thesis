# The main goal of this script is to inspect the content of a station HSD series,
# just to get used to the format and the content. What we want to do here is to
# start developing procedures to distinguish between well-behaved datas and faulty ones.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor")
fname <- "Dataset/ok_stations_dem_30.dat"



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
con <- file(fname, open="r") 

num_lines <- 0
repeat{
  
  # Reading line and getting ready to close file connection if the first one
  # is already a numeric(0), which is the value returned when no item is found
  appo <- readLines(con, n = 1L)
  if(length(appo) == 0) break
  
  # If we got here, it means that the line wasn't void. I will add one to number
  # of lines
  num_lines <- num_lines + 1
}
close(con)

print(paste0("The file we are dealing with has ", num_lines, " lines!"))
print(paste0("This means that it contains datas for ", num_lines/12, " years!"))