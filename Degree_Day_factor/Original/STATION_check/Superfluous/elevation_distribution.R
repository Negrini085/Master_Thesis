# The main goal of this script is to check elevation distribution of my stations, 
# in order to later adopt elevation-wise procedure.
rm(list = ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/Degree_Day_factor/Ours/STATION_check/")


# Importing station properties, in order to later make a plot
fname <- "../Dataset/ANAGRAFICA"
df <- read.table(fname, header = FALSE)
ele <- as.numeric(df$V4)


# Plotting procedure
breaks <- seq(0, max(ele) + 100, by = 100)
df_hist <- data.frame(ele = ele)

ggplot(df_hist, aes(x = ele, y = after_stat(count / sum(count) * 100))) +
  geom_histogram(breaks = breaks, fill = "#3a7abf", color = "white", linewidth = 0.2) +
  labs(
    title = "Station elevation distribution",
    x = "Elevation (m a.s.l.)",
    y = "Frequency (%)"
  ) +
  scale_x_continuous(
    breaks = seq(0, max(ele) + 500, by = 500),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor.y = element_line(color = "grey92", linewidth = 0.2)
  )