# The main goal of this script is to put to zero everything under 2 cm in order 
# to clean a lot of additional and still present noise.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/QC_series/")


# Importing station names
files <- list.files(path = "../Dataset", full.names = TRUE)
station_names <- sub("../Dataset/", "", files)


# Cycle over station names
for(name in station_names){
  
  # Importing hs series
  fname <- paste0("../Dataset/", name)
  df_hs <- read.table(fname)
  
  hs_series <- as.numeric(df_hs$V2)
  hs_years <- as.numeric(df_hs$V1)
  
  
  # Selecting HS datas under 1 cm
  mask <- hs_series <= 1
  hs_series[mask] <- 0
  
  
  # Saving new series
  df_print <- data.frame(year = hs_years, hs = hs_series)
  write.table(df_print, paste0("../Dataset/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
}