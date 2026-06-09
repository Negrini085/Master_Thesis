rm(list = ls())
gc()

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/What/")

files <- list.files(path = "../HS_series/HS_correction/Dataset/model_runs/raw/PCPD", full.names = TRUE)
station_names <- sub("../HS_series/HS_correction/Dataset/model_runs/raw/PCPD/DV_HSD", "DV_SNW", files)

for(name in station_names){
  
  fname <- paste0("../HS_series/HS_correction/Dataset/model_runs/raw/SNWD/", name)
  df <- read.table(fname, header = FALSE)
  
  mask <- as.numeric(df$V5) == -90
  df <- df[!mask, ]
  
  write.table(df, paste0("Dataset/SNW/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made a copy for ", name))
}