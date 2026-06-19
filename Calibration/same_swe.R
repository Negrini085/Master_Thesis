# The main goal of this script is to check if changing model script (from ddf_min 
# and ddf_max to ddf_ave and ddf_ampl) has some effetcs (it should not).
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Calibration/")

files <- list.files(path = "Results/before", full.names = TRUE)
files <- sub("Results/before/", "", files)

for(name in files){
  
  # Importing swe series
  fname_bef <- paste0("Results/before/", name)
  df_bef <- read.table(fname_bef, header = FALSE)
  
  fname_aft <- paste0("Results/after/", name)
  df_aft <- read.table(fname_aft, header = FALSE)
  
  mask <- as.numeric(df_bef$V4) == as.numeric(df_aft$V4)
  if(!all(mask)) stop(paste0("Non compatible swe series for ", name))
}