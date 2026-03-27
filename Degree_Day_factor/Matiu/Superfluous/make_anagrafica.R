# The main goal of this script is to make an anagrafica like ours, to make it 
# easier to adjust code to this new contest.
rm(list = ls())
gc()

fname_anag <- "Dataset/csv/meta_all.csv"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/")


# Importing meta-datas in csv format
df_csv <- read.csv(fname_anag)
print(df_csv)


# Creating new dataframe
df_print <- data.frame(
  name = paste0(df_csv$Provider, '_', df_csv$Name), 
  lon = as.numeric(df_csv$Longitude), 
  lat = as.numeric(df_csv$Latitude), 
  ele = as.numeric(df_csv$Elevation), 
  start = as.numeric(df_csv$HS_year_start), 
  end = as.numeric(df_csv$HS_year_end) 
)

write.table(df_print, "Dataset/data/ANAGRAFICA", row.names = FALSE, col.names = FALSE, quote = FALSE)