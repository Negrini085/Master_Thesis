# The main goal of this script is to make data files starting from csv, in order
# to be able to use the same procedures I was using before
rm(list = ls())
gc()

fname_csv <- "Dataset/csv/data_daily_IT_VDA_CF.csv"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/")


# Importing dataset
df_csv <- read.csv(fname_csv)
station_names <- unique(df_csv$Name)
station_provider <- unique(df_csv$Provider)


# Cycle over station names
for(name in station_names){
  
  # Selecting the part of the data-frame we actually need
  mask <- df_csv$Name == name
  appo_df <- df_csv[mask, ]
  
  # Creating HS file
  df_print <- data.frame(
    year = sapply(strsplit(appo_df$Date, "-"), `[`, 1),
    hs_series = as.numeric(appo_df$HS_after_gapfill)
  )
  
  write.table(df_print, paste0("Dataset/data/", station_provider, "_", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Saved file for station ", name))
}