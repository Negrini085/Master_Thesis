# The main goal of this script is to inspect those hydrological years which show
# a moderate snow coverage (below 200 days) according to MODIS, but almost a full
# year worth of snow coverage according to station datas.
rm(list = ls())
gc()

fname_not_consider <- "Datas/MOD200_STA350.dat"
fname_MOD <- "../MODIS_series/Datas/compatible/los.dat"
fname_STA <- "../STATION_series/Datas/los_compatible.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/FindBug/")


# Importing LOS values computed starting from MODIS and in-situ series
df <- read.table(fname_MOD, header = FALSE)
year_MOD <- as.numeric(df$V2)
los_MOD <- as.numeric(df$V3)
name_MOD <- df$V1

df <- read.table(fname_STA, header = FALSE)
year_STA <- as.numeric(df$V2)
los_STA <- as.numeric(df$V3)
name_STA <- df$V1

df <- read.table(fname_not_consider)
name_no_consider <- unique(df$V1)



# Taking only datas which appear in both datasets
df_MOD <- data.frame(name = name_MOD, year = year_MOD, los = los_MOD)
df_STA <- data.frame(name = name_STA, year = year_STA, los = los_STA)

merged <- merge(df_MOD, df_STA, by = c("name", "year"), suffixes = c("_MOD", "_STA"))
print(merged)


# Now I want to filter datas in order to only take care of those hydroloical years
# which have MODIS LOS smaller than 200 and STATION LOS bigger than 300, without 
# taking into consideration the stations that had passed more stringent checks
mask <- merged$los_MOD < 200 & merged$los_STA > 300
to_analyze <- merged[mask, ]
to_analyze <- to_analyze[!(to_analyze$name %in% name_no_consider), ]

write.table(to_analyze, "Datas/MOD200_STA300.dat", row.names = FALSE, col.names = FALSE, quote = FALSE)