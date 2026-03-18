# The main goal of this script is to produce a plot that enables the user to visually
# compare LOS derived from MODIS series and from in-situ datas for all stations
rm(list = ls())
gc()

library(ggplot2)

fname_STA_comp     <- "../STATION_series/Datas/longest_periods_sc_compatible.dat"
fname_MOD_non_comp <- "../MODIS_series/Datas/non_compatible/longest_periods_sc.dat"
fname_STA_non_comp <- "../STATION_series/Datas/longest_periods_sc_non_compatible.dat"
fname_MOD_comp     <- "../MODIS_series/Datas/compatible/longest_periods_sc_correct.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Comparison/")


# Helper function to import, merge and label a pair of files
load_and_merge <- function(fname_MOD, fname_STA, label){
  df_MOD <- read.table(fname_MOD, header = FALSE)
  df_MOD <- data.frame(
    name = df_MOD$V1,
    year = as.numeric(df_MOD$V2),
    csc  = as.numeric(df_MOD$V3)
  )
  
  df_STA <- read.table(fname_STA, header = FALSE)
  df_STA <- data.frame(
    name = df_STA$V1,
    year = as.numeric(df_STA$V2),
    csc  = as.numeric(df_STA$V3)
  )
  
  merged <- merge(df_MOD, df_STA, by = c("name", "year"), suffixes = c("_MOD", "_STA"))
  merged$group <- label
  return(merged)
}



# Loading and merging both pairs
merged_comp     <- load_and_merge(fname_MOD_comp,     fname_STA_comp,     "Compatible")
merged_non_comp <- load_and_merge(fname_MOD_non_comp, fname_STA_non_comp, "Non-compatible")



# Plotting procedure
df_plot <- rbind(merged_comp, merged_non_comp)

ggplot(df_plot, aes(x = csc_MOD, y = csc_STA, color = group)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(
    values = c("Compatible" = "steelblue", "Non-compatible" = "tomato"),
    name   = "Station type"
  ) +
  labs(
    title = "MODIS vs Station CSC comparison",
    x     = "CSC MODIS [days]",
    y     = "CSC Station [days]"
  ) +
  theme_minimal(base_size = 13) +
  coord_equal()