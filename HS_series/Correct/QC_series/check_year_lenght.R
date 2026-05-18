# The main goal of this script is to check whether some years are not long enough
# or have more days than expected. That could be a result of manual HS corrections.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/")
files <- list.files(path = "Dataset", full.names = TRUE)
files <- sub("Dataset/", "", files)



# Cycle across stations
for(name in files){
  
  # Importing hs series
  fname <- paste0("Dataset/", name)
  df <- read.table(fname)
  
  # Selecting HS values and years
  hs_series <- as.numeric(df$V2)
  hs_years <- as.numeric(df$V1)
  
  # Cycle over station years
  for(y in unique(hs_years)){
    
    # Selecting HS values for a given year
    mask <- hs_years == y
    appo <- hs_series[mask]
    
    len <- length(appo)
    if((y %% 4 != 0) & (len == 365)) next
    if((y %% 4 == 0) & (len == 366)) next
    cat("Wrong year lenght for", name, paste0(" during ", y, ":"), len, "days are present!", "\n")
  }
  
  
}