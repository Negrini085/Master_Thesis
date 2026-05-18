# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for all stations
rm(list = ls())
gc()

library(ggplot2)

fname_MOD     <- "../MODIS_series/Results/csc.dat"
fname_STA     <- "../STATION_series/Results/csc.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Original/Comparison/")


# Importing both dataset
df_MOD <- read.table(fname_MOD, header = TRUE)
df_MOD <- data.frame(name = df_MOD$station, year = as.numeric(df_MOD$year), csc = as.numeric(df_MOD$duration), mark = df_MOD$flag)

df_STA <- read.table(fname_STA, header = FALSE)
df_STA <- data.frame(name = df_STA$V1, year = as.numeric(df_STA$V2), csc = as.numeric(df_STA$V3), mark = df_STA$V5)


# Plotting procedure
df_plot <- merge(df_MOD, df_STA, by = c("name", "year", "mark"), suffixes = c("_MOD", "_STA"))
mask <- !is.na(df_plot$csc_STA)
df_plot <- df_plot[mask, ]

ggplot(df_plot, aes(x = csc_MOD, y = csc_STA, color = mark)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("OK" = "steelblue", "NO" = "tomato"),
    name   = "Recovered"
  ) +
  labs(
    title = "MODIS vs Station CSC comparison",
    x     = "CSC MODIS [days]",
    y     = "CSC Station [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()