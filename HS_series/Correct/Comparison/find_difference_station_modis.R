# The main goal of this script is to find which hydrological years sit over a 
# certain straight line in the los scatter plot. I will indeed filter out those
# datas using the equation y > 1.3x. I will also avoid the region close to the 
# origin of the axis.
rm(list = ls())
gc()

fname_MOD     <- "../MODIS_series/Results/los.dat"
fname_STA     <- "../STATION_series/Results/los.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/Comparison/")


# Importing both dataset
df_MOD <- read.table(fname_MOD, header = TRUE)
df_MOD <- data.frame(name = df_MOD$station, year = as.numeric(df_MOD$year), los = as.numeric(df_MOD$los), mark = df_MOD$flag)

df_STA <- read.table(fname_STA, header = FALSE)
df_STA <- data.frame(name = df_STA$V1, year = as.numeric(df_STA$V2), los = as.numeric(df_STA$V3), mark = df_STA$V4)

df_plot <- merge(df_MOD, df_STA, by = c("name", "year", "mark"), suffixes = c("_MOD", "_STA"))


# Filtering hydrological years
mask <- !(df_plot$los_STA < 10 & df_plot$los_MOD < 10)
df_plot <- df_plot[mask, ]

mask <- df_plot$los_STA > 1.3 * df_plot$los_MOD
df_plot <- df_plot[mask, ]


# Saving df
write.table(df_plot, "Dataset/over_1.3.dat", row.names = FALSE, col.names = TRUE, quote = FALSE)