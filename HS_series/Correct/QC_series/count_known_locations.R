# The main goal of this script is to check how many stations of known locations 
# are present in out dataset.
rm(list = ls())
gc()

fname_new <- "STATION_check/Dataset/ANAGRAFICA"
fname_old <- "../Original/STATION_check/ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/")



# Importing previous and latest inventory of AWS
df_old <- read.table(fname_old, header = TRUE)
df_new <- read.table(fname_new, header = TRUE)



# Status of previous dataset
mask <- !(df_old$flag == "NO")
cat("PREVIOUS DATASET: ", "\n", "\n")
cat("Stations of known location: ", sum(mask, na.rm = TRUE), "\n")
cat("Stations of un-known location: ", sum(!mask, na.rm = TRUE), "\n")
cat("Station of known location are", sum(mask, na.rm = TRUE)/length(mask)*100, "% of total dataset!", "\n")



# Status of current dataset
cat( "\n", "\n",  "\n")
mask <- !(df_new$flag == "NO")
cat("CURRENT DATASET: ", "\n", "\n")
cat("Stations of known location: ", sum(mask, na.rm = TRUE), "\n")
cat("Stations of un-known location: ", sum(!mask, na.rm = TRUE), "\n")
cat("Station of known location are", sum(mask, na.rm = TRUE)/length(mask)*100, "% of total dataset!", "\n")