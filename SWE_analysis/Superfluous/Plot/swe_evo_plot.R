rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SWE_analysis/")

swe <- read.table("Results/masked_swe_evolution.dat", header = TRUE)
swe <- swe$swe

df <- data.frame(
  day = seq_along(swe),
  swe = swe
)

data0 <- as.Date("1951-10-01")
dates <- as.Date(paste0(seq(1955, 2020, by = 5), "-01-01"))
dates_ind <- as.numeric(dates - data0) + 1

p <- ggplot(df, aes(x = day, y = swe)) +
  geom_line(
    linewidth = 1.2,
    colour = "#1F4E79",
    lineend = "round"
  ) +
  scale_x_continuous(
    breaks = dates_ind,
    labels = format(dates, "%Y"),
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    x = "Date",
    y = expression(bold("SWE [Gm"^3*"]"))
  ) +
  theme_classic(base_size = 12) +
  theme(
    text = element_text(
      family = "sans",
      colour = "black"
    ),
    axis.title = element_text(
      size = 22,
      face = "bold"
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    axis.text = element_text(
      size = 18,
      colour = "black"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.line = element_line(
      linewidth = 0.6,
      colour = "black"
    ),
    axis.ticks = element_line(
      linewidth = 0.5,
      colour = "black"
    ),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

ggsave(
  "Images/masked_swe_evolution.png",
  plot = p,
  width = 14,
  height = 6,
  units = "in",
  dpi = 600
)