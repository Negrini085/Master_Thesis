# The main goal of this script is to convert our MODIS series to hydrological ones
# in order to compare the two methods of extraction.
rm(list = ls())
gc()

fname_mat <- "Dataset/MODIS_on_stations"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/MODIS_vs_HS/")


# Importing MODIS series
df <- read.table(fname_mat, header = TRUE)
df <- df[191:(nrow(df)-122), ]


for(name in colnames(df)){
  
  # HS series and years
  hs_datas <- as.numeric(df[[name]])
  dates <- seq(from = as.Date("2001-01-01"), to = as.Date("2023-12-31"), by = "day")
  years <- as.numeric(format(dates, "%Y"))
  
  # Data-frame creation
  if(length(hs_datas) != length(years)) stop(paste0("Non compatible lenght for ", name, ": ", length(years), " & ", length(hs_datas)))
  df_print <- data.frame(year = years, value = hs_datas)
  
  # Saving datas
  write.table(df_print, paste0("Dataset/sc_hydro/", name), row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(paste0("Made conversion for ", name))
}