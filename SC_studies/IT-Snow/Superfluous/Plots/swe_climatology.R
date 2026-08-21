# The main goal of this script is to create a climatology plot for SWE total 
# volume across an hydrological year. I would also like to show quantiles of the
# distribution.
rm(list = ls())
gc()

library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SC_studies/IT-Snow/")

swe <- read.table("Datas/swe_evolution.dat")
swe <- swe$V1

years <- 2011:2025
hydro_days <- 365

swe_hydro <- numeric(0)
index <- 0

for (year in years) {
  
  if (year %% 4 == 0) {
    swe_year <- swe[(index + 1):(index + hydro_days + 1)]
    
    swe_hydro <- c(
      swe_hydro,
      swe_year[1:180],
      mean(swe_year[181:182]),
      swe_year[183:(hydro_days + 1)]
    )
    
    index <- index + hydro_days + 1
    
  } else {
    swe_hydro <- c(
      swe_hydro,
      swe[(index + 1):(index + hydro_days)]
    )
    
    index <- index + hydro_days
  }
}

if (length(swe_hydro) != hydro_days * length(years)) {
  stop("The SWE time series does not match the expected number of hydrological days.")
}

swe_mat <- matrix(
  swe_hydro,
  nrow = hydro_days,
  ncol = length(years),
  byrow = FALSE
)

colnames(swe_mat) <- years

clim <- data.frame(
  day = seq_len(hydro_days),
  q25 = apply(
    swe_mat,
    1,
    quantile,
    probs = 0.25,
    na.rm = TRUE
  ),
  median = apply(
    swe_mat,
    1,
    quantile,
    probs = 0.50,
    na.rm = TRUE
  ),
  q75 = apply(
    swe_mat,
    1,
    quantile,
    probs = 0.75,
    na.rm = TRUE
  ),
  date = seq(
    as.Date("2025-09-01"),
    by = "day",
    length.out = hydro_days
  )
)

p <- ggplot(clim, aes(x = date)) +
  geom_ribbon(
    aes(ymin = q25, ymax = q75),
    fill = "#E6A66A",
    alpha = 0.45
  ) +
  
  geom_line(
    aes(y = q25),
    linewidth = 0.8,
    colour = "#D17A22",
    lineend = "round"
  ) +
  
  geom_line(
    aes(y = median),
    linewidth = 1.2,
    colour = "#1F4E79",
    lineend = "round"
  ) +
  
  geom_line(
    aes(y = q75),
    linewidth = 0.8,
    colour = "#D17A22",
    lineend = "round"
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("2025-10-01"),
      as.Date("2026-08-01"),
      by = "2 months"
    ),
    labels = format(
      seq(
        as.Date("2025-10-01"),
        as.Date("2026-08-01"),
        by = "2 months"
      ),
      "%b"
    ),
    expand = expansion(mult = c(0.005, 0.005))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04))
  ) +
  
  labs(
    x = "Hydrological day",
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
  "Images/swe_climatology.png",
  plot = p,
  width = 14,
  height = 7,
  units = "in",
  dpi = 600
)