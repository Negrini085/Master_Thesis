# The main goal of this script is to check elevation distribution of my stations, 
# in order to later adopt elevation-wise procedure.
rm(list = ls())
gc()
library(ggplot2)
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Correct/STATION_check/")

fname <- "Dataset/ANAGRAFICA"
df <- read.table(fname, header = TRUE)
ele <- as.numeric(df$ele_rev)

breaks <- seq(0, max(ele) + 100, by = 100)
df_hist <- data.frame(ele = ele)

ggplot(df_hist, aes(y = ele, x = after_stat(count / sum(count) * 100))) +
  geom_histogram(breaks = breaks, fill = "#3a7abf", color = "white",
                 linewidth = 0.2, orientation = "y") +
  labs(
    title = "AWS elevation distribution: after QC",
    y = "Elevation (m a.s.l.)",
    x = "Frequency (%)"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(ele) + 500, by = 500),
    limits = c(0, 3500),
    expand = c(0.01, 0)
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey92", linewidth = 0.2)
  )