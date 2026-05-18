# The main goal of this script is to find spikes in HS readings. I should have 
# already taken care of those kind of evenience, but maybe something went through.
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
  
  # Cycle across station years
  for(y in unique(hs_years)){
    
    # Selecting datas for a given year
    mask <- hs_years == y
    appo <- hs_series[mask]
    
    # Looking for spikes
    for(i in 2:(length(appo)-1)){
      pre <- appo[i] - appo[i-1]
      post <- appo[i] - appo[i+1]
      
      if((pre > 100) & (post > 100)) cat("Spike present for", name, "during", y, "\n")
    }
  }
  
}