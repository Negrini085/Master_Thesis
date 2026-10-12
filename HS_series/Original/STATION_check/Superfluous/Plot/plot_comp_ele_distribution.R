# The main goal of this script is to plot station compatibility and elevation distribution, in order to give a first impression on what the HS dataset actually looks like.
rm(list = ls())
gc()

library(sf)
library(ggplot2)
library(patchwork)
library(rnaturalearth)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/HS_series/Original/STATION_check/")



# Making first plot
appo <- read.table("Dataset/check_ele.dat", header = TRUE, fill = TRUE)
df_plot <- data.frame(lon = as.numeric(appo$lon), lat = as.numeric(appo$lat), diff = as.numeric(appo$diff), status = appo$mark)
df_plot <- na.omit(df_plot)


europe <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf")

p1 <- ggplot() +
  geom_sf(data = europe, fill = "antiquewhite1", color = "grey70") +
  geom_point(
    data = df_plot,
    aes(x = lon, y = lat, color = status),
    size = 1.5, alpha = 0.9
  ) +
  scale_color_manual(
    values = c("OK" = "forestgreen", "NO" = "firebrick1"),
    name = "Reliability"
  ) +
  coord_sf(xlim = c(3.5, 17), ylim = c(43, 49), expand = FALSE) +
  theme_minimal() +
  labs(
    title = NULL,
    x = "Longitude [°E]",
    y = "Latitude [°N]"
  ) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    axis.title = element_text(size = 20,  margin = margin(t = 10)),
    axis.text = element_text(size = 15)
  )



p2 <- ggplot(df_plot, aes(x = diff)) +
  geom_histogram(
    aes(
      y = after_stat(count / sum(count) * 100),
      fill = after_stat(abs(x) <= 12),
      color = after_stat(abs(x) <= 12)
    ),
    binwidth = 3,
    boundary = 0
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick1"
    ),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick1"
    ),
    guide = "none"
  ) +
  coord_cartesian(xlim = c(-500, 500)) +
  labs(
    x = "Difference [m a.s.l]",
    y = "Frequency (%)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15)
  )



p1 <- p1 +
  theme(plot.margin = margin(t = 5, r = 25, b = 5, l = 5))

p2 <- p2 +
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 25))

(p1 | p2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(size = 15, face = "bold"),
    plot.tag.position = c(0.05, 0.95),
    legend.title = element_text(size =20),
    legend.text = element_text(size = 15)
  )