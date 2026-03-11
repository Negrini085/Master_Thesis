# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for compatible (or 
# non-compatible stations, according to the user needs)
rm(list = ls())
gc()

library(ggplot2)

fname_STA <- "../STATION_series/Datas/longest_periods_sc_non_compatible.dat"
fname_MOD <- "../MODIS_series/Datas/non_compatible/longest_periods_sc.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")



# Importing datas
df <- read.table(fname_MOD, header = FALSE)
year_MOD <- as.numeric(df$V2)
csc_MOD <- as.numeric(df$V3)
name_MOD <- df$V1

df <- read.table(fname_STA, header = FALSE)
year_STA <- as.numeric(df$V2)
csc_STA <- as.numeric(df$V3)
name_STA <- df$V1



# Taking only datas which appear in both datasets
df_MOD <- data.frame(name = name_MOD, year = year_MOD, csc = csc_MOD)
df_STA <- data.frame(name = name_STA, year = year_STA, csc = csc_STA)

merged <- merge(df_MOD, df_STA, by = c("name", "year"), suffixes = c("_MOD", "_STA"))



# Plotting procedure
data_MOD <- merged$csc_MOD
data_STA <- merged$csc_STA
df_plot <- data.frame(MOD = data_MOD, STA = data_STA)

ggplot(df_plot, aes(x = MOD, y = STA)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title    = "MODIS vs Station CSC comparison",
    x        = "CSC MODIS [days]",
    y        = "CSC Station [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()