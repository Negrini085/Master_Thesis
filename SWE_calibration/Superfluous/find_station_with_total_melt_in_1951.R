# The main goal of this script is to find those stations which have at least one 
# day without snow coverage during 1951 in Michele series, in order to ensure that 
# all the data which we are comparing are not biased.
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_calibration/")

files <- list.files(path = "Results/raw", full.names = TRUE)
station_names <- sub("Results/raw/", "", files)
station_names <- station_names[startsWith(station_names, "V_SDH")]


# Cycle over stations
list <- character(0)
for(name in station_names){
  
  # Importing Michele series
  fname_MICH <- paste0("../Original_series/FILTERED_SNW/", name)
  df_MICH <- read.table(fname_MICH, header = FALSE)
  
  mask <- as.numeric(df_MICH$V1) == 1951
  df_MICH <- df_MICH[mask, ]
  
  # Checking if everything melts
  appo_MICH <- as.numeric(df_MICH$V5)
  if(all(appo_MICH != 0)){
    print(name)
    next
  }
  list <- c(list, sub("V_SDH", "HSD", name))
}

df <- data.frame(name = list)
write.table(df, "Results/LIST_TO_COMPARE", row.names = FALSE, col.names = FALSE, quote = FALSE)