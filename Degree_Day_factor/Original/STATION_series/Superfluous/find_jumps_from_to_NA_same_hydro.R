# The main goal of this script is to find those hydrological years which have
# more than one jump from/to NA values. I want to do that in order to find
# some hydrological years which contain both positive and negative jumps, hoping
# to be able to retrieve some of them.
rm(list = ls())
gc()

fname_negative <- "Datas/results/na_or_zero_filter/negative_jump_to_NA.dat"
fname_positive <- "Datas/results/na_or_zero_filter/positive_jump_from_NA.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/STATION_series/")


# Importing positive and negative jump data-frames
df_positive <- read.table(fname_positive, header = FALSE)
df_negative <- read.table(fname_negative, header = FALSE)


# Filtering on station names and years
key_positive <- paste(df_positive$V1, df_positive$V2, sep = "_")
key_negative <- paste(df_negative$V1, df_negative$V2, sep = "_")

df_positive <- df_positive[key_positive %in% key_negative, ]
df_negative <- df_negative[key_negative %in% key_positive, ]


# Sorting jumps and creating joint dataframe
jump_pos <- numeric(0)
jump_years <- numeric(0)
jump_flag <- character(0)
jump_names <- character(0)
station_names <- unique(df_positive$V1)
for(name in station_names){
  
  # Extracting positive jumps properties
  mask <- df_positive$V1 == name
  positive_pos <- as.numeric(df_positive$V3)[mask]
  positive_years <- as.numeric(df_positive$V2)[mask]
  
  # Extracting negative jumps properties
  mask <- df_negative$V1 == name
  negative_pos <- as.numeric(df_negative$V3)[mask]
  negative_years <- as.numeric(df_negative$V2)[mask]
  
  
  # Cycle to link together jumps
  for(year in unique(positive_years)){
    
    # Selecting positive jumps
    mask <- positive_years == year
    appo_position <- positive_pos[mask]
    flag <- rep("POS", sum(mask, na.rm = TRUE))
    
    # Selecting negative jumps
    mask <- negative_years == year
    appo_position <- c(appo_position, negative_pos[mask])
    flag <- c(flag, rep("NEG", sum(mask, na.rm = TRUE)))
    
    # Sorting jumps from first to last
    ord <- order(appo_position)
    appo_position <- appo_position[ord]
    flag <- flag[ord]
    
    # Checking if series starts as a faulty one (or ends like that)
    if(flag[1] == "POS" | flag[length(flag)] == "NEG"){
      next
    }
    else{
      jump_flag <- c(jump_flag, flag)
      jump_pos <- c(jump_pos, appo_position)
      jump_years <- c(jump_years, rep(year, length(appo_position)))
      jump_names <- c(jump_names, rep(name, length(appo_position)))
    }
  }
}


# Saving to file
df_common <- data.frame(
  names = jump_names, 
  years = jump_years, 
  position = jump_pos, 
  flag = jump_flag
)

write.table(df_common, "Datas/results/na_or_zero_filter/common_jumps_from_to_NA.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)