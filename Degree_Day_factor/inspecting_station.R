# The main goal of this script is to inspect the content of a station HSD series,
# just to get used to the format and the content. What we want to do here is to
# start developing procedures to distinguish between well-behaved datas and faulty ones.
rm(list = ls())
gc()

library(ggplot2)

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



# Function to read the whole content of a station file.
read_station <- function(fname) {
  
  # Reading every line using as item separator every possible white space (one or
  # more). Having "fill = TRUE" helps us to avoid problems related to different 
  # number of days in different months!
  df <- read.table(fname, header = FALSE, sep = "", fill = TRUE)
  
  # Here we are converting dataframe into a matrix and then dropping the first two
  # columns, namely being year and month
  raw <- as.matrix(df[, -(1:2), drop = FALSE])
  
  # Creating hs_series using unlist to concatenate matrix lines. Here we have to 
  # be careful, because we want to get rid of NAs added to make the dataframe 
  # rectangular, while at the same time converting -90 in NAs 
  hs_series <- unlist(lapply(seq_len(nrow(raw)), function(i) {
    x <- raw[i, ]
    x <- x[!is.na(x)]
    x[x == -90] <- NA
    x
  }), use.names = FALSE)
  
  hs_series
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
hs_piamprato <- read_station(fname = fname)



# Plotting procedure
df <- data.frame(
  day = 1:length(hs_piamprato),
  hs = hs_piamprato
)

ggplot(df, aes(x = day, y = hs)) + 
  geom_line(color = "blue", linewidth = 1.5) +
  labs(title = "AWS Piamprato: 1993 to 2022", x = "Days", y = "HS [cm]") +
  theme_minimal()
