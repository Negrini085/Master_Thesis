# The main goal of this script is to study the inversions in mean temperature order
rm(list = ls())
gc()

fname <- "total_inversions.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_model/QC_inputs/TEMP/")



# Importing inversion dataset
df <- read.table(fname, header = FALSE)



# Checking whether leap year appear in this dataset
years <- unique(as.numeric(df$V1))
mask <- years %% 4 == 0
if(any(mask)) print(paste0("Leap years are present in this dataset!"))



# Checking if every day of the year has this kind of inversions
for(y in years){
  
  # Selecting days
  mask <- as.numeric(df$V1) == y
  days <- unique(as.numeric(df$V4)[mask])
  if(length(days) == 365) print(paste0("All days have inversions in ", y))
}



# Checking if inversions take place in the same number of days
for(y in years){
  
  # Selecting days
  mask <- as.numeric(df$V1) == y
  days <- unique(as.numeric(df$V4)[mask])
  
  # Checking length
  cat("Days with inversions: ", length(days), "     Years: ", y, "\n")
}
cat("\n\n\n")
rm(mask, days); invisible(gc());


# Checking if inversion take place in the same location every day
for(y in years){
  
  # Selecting days
  mask <- as.numeric(df$V1) == y
  appo_lon <- as.numeric(df$V2)[mask]
  appo_lat <- as.numeric(df$V3)[mask]
  appo_days <- as.numeric(df$V4)[mask]
  
  mask <- appo_days == appo_days[1]
  comp_lon <- appo_lon[mask]
  comp_lat <- appo_lat[mask]
  
  for(day in appo_days[2:length(appo_days)]){
    
    mask <- appo_days == day
    lon <- appo_lon[mask]
    lat <- appo_lat[mask]
    
    if(length(comp_lon) != length(lon)) print("Different length of longitude vals!") 
    if(length(comp_lat) != length(lat)) print("Different length of latitude vals!")
    
  }
}