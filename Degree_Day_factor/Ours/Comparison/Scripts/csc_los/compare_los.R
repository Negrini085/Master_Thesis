# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for compatible (or 
# non-compatible stations, according to the user needs)
rm(list = ls())
gc()

library(ggplot2)

fname_MOD <- "../MODIS_series/Datas/compatible/los.dat"
fname_STA <- "../STATION_series/Datas/los_compatible.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")



# Importing datas
df <- read.table(fname_MOD, header = FALSE)
year_MOD <- as.numeric(df$V2)
los_MOD <- as.numeric(df$V3)
name_MOD <- df$V1

df <- read.table(fname_STA, header = FALSE)
year_STA <- as.numeric(df$V2)
los_STA <- as.numeric(df$V3)
name_STA <- df$V1



# Taking only datas which appear in both datasets
df_MOD <- data.frame(name = name_MOD, year = year_MOD, los = los_MOD)
df_STA <- data.frame(name = name_STA, year = year_STA, los = los_STA)

merged <- merge(df_MOD, df_STA, by = c("name", "year"), suffixes = c("_MOD", "_STA"))



# Plotting procedure
data_MOD <- merged$los_MOD
data_STA <- merged$los_STA
df_plot <- data.frame(MOD = data_MOD, STA = data_STA)

ggplot(df_plot, aes(x = MOD, y = STA)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title    = "MODIS vs Station LOS comparison",
    x        = "LOS MODIS [days]",
    y        = "LOS Station [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()