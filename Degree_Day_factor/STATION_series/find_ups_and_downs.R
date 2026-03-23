# The main goal of this script is to look for sudden changes in snow depth. I will 
# focus on two different kind of jumps, those who are consecutive and opposite and
# those which are just so big that are a little bit sus. As an example of the first
# category, I'm interested in a situation like the following [40, 80, 30], which is 
# clearly related to a malfunction. I feel like I will also have a minimum height of
# the step in order to be taken care of. Now, as an example of the second category, 
# I want to deal with series like [30, 210, 215], which is also clearly not reliable.
# DeltaH max will be something like 100 cm, in order to be almost impossible for a 
# typical snowfall to be achieved. The main thing to keep in mind is that I WANT TO
# TAKE CARE IN THIS SCRIPT ONLY OF FINITE HS VALUES, SO IF A DAY IS MISSING IT'S NOT
# A STEP THAT WILL BE HANDLED HERE.
rm(list = ls())
gc()

fname_usable <- "Datas/station_series/na_or_zero_filter/usable_stations_na_or_zero.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing names of usable stations
df <- read.table(fname_usable, header = FALSE)
station_names <- df$V1
jump_max <- 100
jump_min <- 20



#------------------------------------------------------------------------------#
#                                UP & DOWN JUMPS                               #
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
  
  # Evaluating difference in hs series (and filter for huge jumps)
  hs_diff <- diff(hs_series)
  big_enough <- abs(hs_diff[-length(hs_diff)]) > jump_min
  opposite_sign <- (hs_diff[-length(hs_diff)] * hs_diff[-1]) < 0
  similar_amp <- abs(abs(hs_diff[-length(hs_diff)]) - abs(hs_diff[-1]))/abs(hs_diff[-length(hs_diff)]) < 0.20
  mask_zigzag <- !is.na(opposite_sign) & !is.na(similar_amp) & opposite_sign & similar_amp & big_enough
  
  # Saving huge jump station and position
  if(sum(mask_zigzag, na.rm = TRUE) != 0){
    positions <- which(mask_zigzag)
    jump_pos   <- c(jump_pos,   positions)
    jump_years <- c(jump_years, hs_years[positions + 1])
    jump_names <- c(jump_names, rep(name, sum(mask_zigzag)))
  }
}

# Saving to file
df_print <- data.frame(
  names = jump_names, 
  years = jump_years, 
  positions = jump_pos
)

write.table(df_print, "Datas/results/na_or_zero_filter/up_down_jumps.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)











#------------------------------------------------------------------------------#
#                     SUDDEN JUMPS ABOVE GIVEN THRESHOLD                       #
#------------------------------------------------------------------------------#
jump_pos <- integer(0)
jump_years <- numeric(0)
jump_names <- character(0)
for(name in station_names){
  
  # Importing HS series for a given station
  fname <- paste0("Datas/station_series/na_or_zero_filter/", name)
  df <- read.table(fname, header = FALSE)
  hs_series <- as.numeric(df$V2)
  hs_years <- as.numeric(df$V1)
  
  # Evaluating difference in hs series (and filter for huge jumps)
  hs_diff <- diff(hs_series)
  mask <- !is.na(hs_diff) & abs(hs_diff) > jump_max
  
  # Saving huge jump station and position
  if(sum(mask, na.rm = TRUE) != 0){
    jump_pos <- c(jump_pos, which(mask))
    jump_years <- c(jump_years, hs_years[mask])
    jump_names <- c(jump_names, array(name, dim = c(sum(mask, na.rm = TRUE))))
  }
}

# Saving to file
df_print <- data.frame(
  names = jump_names, 
  years = jump_years, 
  positions = jump_pos
)

write.table(df_print, "Datas/results/na_or_zero_filter/huge_jumps.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)