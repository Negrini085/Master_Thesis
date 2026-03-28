# The main goal of this script is to evaluate when measures started and ended at 
# a specific station.
rm(list = ls())
gc()

fname_anag <- "../Dataset/data/ANAGRAFICA_ITA"
fname_corr <- "../STATION_check/Correcting/correcting_dataset_ita.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/MODIS_series/")


# Importing both dataframes
lon <- numeric(0)
lat <- numeric(0)
end <- numeric(0)
start <- numeric(0)
mark <- character(0)
names <- character(0)
df_anag <- read.table(fname_anag, header = FALSE)
df_corr <- read.table(fname_corr, header = FALSE)


# Cycle over stations
for(name in df_corr$V1){
  
  if(file.exists(paste0("../Dataset/data/", name))){
    names <- c(names, name)
    
    mask <- df_corr$V1 == name
    lon <- c(lon, df_corr$V2[mask])
    lat <- c(lat, df_corr$V3[mask])
    mark <- c(mark, df_corr$V5[mask])
    
    mask <- df_anag$V1 == name
    end <- c(end, df_anag$V6[mask])
    start <- c(start, df_anag$V5[mask])
  }
}



# Building final dataframe and creating a file
df <- data.frame(
  names = names, 
  lon = lon, 
  lat = lat,
  start = start, 
  end = end, 
  mark = mark
)


# Filter in order to consider only suitable stations
mask <- df$end > 2000
df <- df[mask, ]

mask <- df$start < 2000
df$start[mask] <- 2000

mask <- df$start < df$end
df <- df[mask, ]

write.table(df, file = "Dataset/usable_modis.dat", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")