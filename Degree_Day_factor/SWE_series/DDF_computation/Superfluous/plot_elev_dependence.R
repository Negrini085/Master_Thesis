# The main goal of this script is to investigate elevation dependence for DDFs
rm(list = ls())
gc()

library(sf)
library(ncdf4)
library(ggplot2)
library(rnaturalearth)

fname <- "Results/station_ddf.dat"
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/SWE_series/DDF_computation/")


# Importing station ddf and infos
df <- read.table(fname, header = TRUE)
station_ele <- as.numeric(df$ele)
station_ddf <- as.numeric(df$ddf)


# Plotting procedure
df_plot <- data.frame(ele = station_ele, ddf = station_ddf)
df_plot <- na.omit(df_plot)

ggplot(df_plot, aes(x = ele, y = ddf)) +
  geom_point(color = "steelblue", alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  
  labs(title = "Median Degree-Day Factors vs Elevation",
       x = "Elevation [m a.s.l.]",
       y = "Median DDF [mm/(°C · day)]") +
  
  scale_x_continuous(breaks = seq(0, 4000, 500)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "grey92"),
    panel.grid.minor = element_line(color = "grey96", linetype = "dashed"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )