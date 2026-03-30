# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for all stations
rm(list = ls())
gc()

library(ggplot2)

fname_MOD     <- "../MODIS_series/Results/los.dat"
fname_STA     <- "../STATION_series/Result/los.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Matiu/Comparison/")


# Importing both dataset
df_MOD <- read.table(fname_MOD, header = FALSE)
df_MOD <- data.frame(name = df_MOD$V1, year = as.numeric(df_MOD$V2), los = as.numeric(df_MOD$V3), mark = df_MOD$V4)

df_STA <- read.table(fname_STA, header = FALSE)
df_STA <- data.frame(name = df_STA$V1, year = as.numeric(df_STA$V2), los = as.numeric(df_STA$V3), mark = df_STA$V4)


# Plotting procedure
df_plot <- merge(df_MOD, df_STA, by = c("name", "year", "mark"), suffixes = c("_MOD", "_STA"))

ggplot(df_plot, aes(x = los_MOD, y = los_STA, color = mark)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("OK" = "steelblue", "NO" = "tomato"),
    name   = "Recovered"
  ) +
  labs(
    title = "MODIS vs Station LOS comparison",
    x     = "LOS MODIS [days]",
    y     = "LOS Station [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()