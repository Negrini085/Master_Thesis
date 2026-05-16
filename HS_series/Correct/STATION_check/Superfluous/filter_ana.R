# The main goal of this script is to filter ANAGRAFICA file in order to have only
# the remaining stations.
rm(list = ls())
gc()

fname <- "Dataset/OLD_ANAGRAFICA"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/STATION_check/")



# Which stations we have kept?
files <- list.files(path = "../Dataset", full.names = TRUE)
files <- sub("../Dataset/", "", files)



# Filtering stations
df <- read.table(fname, header = TRUE)
station_names <- df$station_name

mask <- station_names %in% files
df <- df[mask, ]



# Saving to file
write.table(df, "Dataset/ANAGRAFICA", row.names = FALSE, col.names = TRUE, quote = FALSE)