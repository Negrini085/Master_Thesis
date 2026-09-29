# The main goal of this script is to plot the area covered all year round by snow
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

df <- read.table("Results/csc_covered_area.dat", header = TRUE)
df <- data.frame(
  year = as.numeric(df$year),
  area = as.numeric(df$area)
)


p <- ggplot(df, aes(x = year, y = area)) +
  geom_line(
    linewidth = 1.2,
    colour = "#1F4E79",
    lineend = "round"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(df$year) / 5) * 5,
      ceiling(max(df$year) / 5) * 5,
      by = 5
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Year",
    y = expression(bold("Area [km"^2*"]"))
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(family = "sans", colour = "black"),
    axis.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text = element_text(size = 18, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.line = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

print(p)