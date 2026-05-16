# The main goal of this script is to count filtered series, in order to asses how
# many we gonna keep as part of our HS dataset
rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/QC_series/")



# Cycle over files
conta <- 0
files <- list.files(path = "Dataset/Final", full.names = TRUE)

for(file in files) {
  df <- read.table(file) 
  appo <- as.numeric(df$V1)
  conta <- conta + length(unique(appo))
}

print(paste0("Actual number of hydrological years is ", conta))