# The main goal of this script is to check whether some station are counted twice. 
# I need to do so in order to be able to handle correctly station series.
rm(list = ls())
gc()

fname_ana <- "ANAGRAFICA_REV"
fname_list <- "../../../SWE_series/Dataset/model_runs/raw/PCPD/lista"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_check/Correcting/")


# Importing ANAGRAFICA
df <- read.table(fname_ana, header = TRUE)
station_names <- df$station_name


# Importing model list
df_ana <- read.table(fname_list)
list_names <- sub("^D_", "", df_ana$V1)
print(paste0("List names: ", length(list_names)))


# Selecting only first occurance of a given station
df <- df[!duplicated(df$station_name), ]
print(paste0("Single occurance stations: ", length(df$station_name)))


# Saving actual correct ANAGRAFICA to file
fname_ana <- "ANAGRAFICA_CORRECT"
write.table(df, fname_ana, row.names = FALSE, col.names = TRUE, quote = FALSE)