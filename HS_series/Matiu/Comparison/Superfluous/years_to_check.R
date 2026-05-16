# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for all stations
rm(list = ls())
gc()

# The main goal of this script is to deal with LOS outliers. In particular we need
# to adress the cause of such a behaviour.
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)

fname_ana <- "../Dataset/data/ANAGRAFICA"
fname_MOD <- "../MODIS_series/Results/los.dat"
fname_STA <- "../STATION_series/Result/los_filtered.dat"
fname_to_omit <- "../MODIS_series/Results/years_to_filter_out.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/Comparison/")


# Importing datasets and registry
df_MOD <- read.table(fname_MOD, header = FALSE)
df_MOD <- data.frame(name = df_MOD$V1, year = as.numeric(df_MOD$V2), los = as.numeric(df_MOD$V3), mark = df_MOD$V4)

df_STA <- read.table(fname_STA, header = FALSE)
df_STA <- data.frame(name = df_STA$V1, year = as.numeric(df_STA$V2), los = as.numeric(df_STA$V3), mark = df_STA$V4)

df_to_omit <- read.table(fname_to_omit, header = FALSE)
df_to_omit <- data.frame(name = df_to_omit$V1, year = as.numeric(df_to_omit$V2), mark = df_to_omit$V3)

df_ana <- read.table(fname_ana, header = FALSE)
df_ana <- data.frame(name = df_ana$V1, ele = df_ana$V4)


# Omitting corrupted datas
df_merged <- merge(df_MOD, df_STA, by = c("name", "year", "mark"), suffixes = c("_MOD", "_STA"))
df_merged <- anti_join(df_merged, df_to_omit, by = c("name", "year", "mark"))
mask <- !is.na(df_merged$los_STA)
df_merged <- df_merged[mask, ]


# Selecting only stations which respect my requests
mask <- (df_merged$los_STA) > 50 & (df_merged$los_STA > (2 * df_merged$los_MOD))
df_to_check <- df_merged[mask, ]


# Evaluating elevation
ele <- numeric(0)
for(name in df_to_check$name){
  mask <- df_ana$name == name
  appo <- as.numeric(df_ana[mask, 2])
  
  if(length(appo) != 1) print("Problem with registry dataset!")
  ele <- c(ele, appo)
}
df_to_check$ele <- ele
write.table(df_to_check, "Results/years_to_check.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)