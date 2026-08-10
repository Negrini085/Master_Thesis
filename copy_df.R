rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Original_series/")

files <- list.files(path = "PCPD", full.names = TRUE)
files <- list.files(path = "PCPD", pattern = "^DV", full.names = TRUE)
station_names <- sub("PCPD/DV_HSD", "V_SNW", files)

for(name in station_names){
  
  fname <- paste0("SNWD/", name)
  df <- read.table(fname, header = FALSE)
  
  mask <- as.numeric(df$V5) == -90
  df <- df[!mask, ]
  
  write.table(df, paste0("FILTERED_SNW/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made a copy for ", name))
}