# The main goal of this script is to find those jumps at the beginning/end of seasons (or 
# even in the middle of it) that could invalid a given hydrological year because I. I will
# divide this check in two phases, the first one focused on negative jumps, whilst the
# other one on positive ones. I want to take care of series like [100, 105, -90, -90]
# or the other way around; it is clear that there is a need to check for the presence of 
# a certain series of missing data in addition to the single jump.
rm(list = ls())
gc()

fname_usable <- "Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing usable stations
df <- read.table(fname_usable, header = FALSE)
station_names <- df$V1
rm(df)
gc()



#------------------------------------------------------------------------------#
#                                NEGATIVE JUMPS                                #
#------------------------------------------------------------------------------#
jump_pos   <- integer(0)
jump_years <- numeric(0)
jump_names <- character(0)
for(name in station_names){
  
  # Importing HS series for a given station
  fname <- paste0("Datas/station_series/na_or_zero_filter/", name)
  df <- read.table(fname, header = FALSE)
  hs_series <- as.numeric(df$V2)
  hs_years <- as.numeric(df$V1)
  
  # Computing differences (using appo variable, because we need to compute differences with NAs)
  appo <- hs_series
  appo[is.na(appo)] <- 0
  hs_diff <- diff(appo)
  
  # Filtering hs differences and finding where those jumps are located
  mask <- hs_diff < -30
  indx <- which(mask) + 1
  
  # Checking whether following 15 days are NAs or not
  for(ind in indx){
    
    # Selecting following values to check
    end_idx <- min(ind + 14, length(hs_series))
    to_check <- hs_series[ind:end_idx]
    
    # Adding to file
    if(all(is.na(to_check))){
      jump_pos <- c(jump_pos, ind)
      jump_names <- c(jump_names, name)
      jump_years <- c(jump_years, hs_years[ind])
    }
  }
}

# Saving to file
df <- data.frame(
  names = jump_names,
  years = jump_years,
  position = jump_pos 
)

write.table(df, "Datas/results/na_or_zero_filter/negative_jump_to_NA.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)











#------------------------------------------------------------------------------#
#                                POSITIVE JUMPS                                #
#------------------------------------------------------------------------------#
jump_pos   <- integer(0)
jump_years <- numeric(0)
jump_names <- character(0)
for(name in station_names){
  
  # Importing HS series for a given station
  fname <- paste0("Datas/station_series/na_or_zero_filter/", name)
  df <- read.table(fname, header = FALSE)
  hs_series <- as.numeric(df$V2)
  hs_years <- as.numeric(df$V1)
  
  # Computing differences (using appo variable, because we need to compute differences with NAs)
  appo <- hs_series
  appo[is.na(appo)] <- 0
  hs_diff <- diff(appo)
  
  # Filtering hs differences and finding where those jumps are located
  mask <- hs_diff > 30
  indx <- which(mask)
  
  # Checking whether following 15 days are NAs or not
  for(ind in indx){
    
    # Selecting following values to check
    start_idx <- max(ind - 14, 1)
    to_check <- hs_series[start_idx:ind]
    
    # Adding to file
    if(all(is.na(to_check))){
      jump_pos <- c(jump_pos, ind)
      jump_names <- c(jump_names, name)
      jump_years <- c(jump_years, hs_years[ind])
    }
  }
}

# Saving to file
df <- data.frame(
  names = jump_names,
  years = jump_years,
  position = jump_pos 
)

write.table(df, "Datas/results/na_or_zero_filter/positive_jump_from_NA.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)