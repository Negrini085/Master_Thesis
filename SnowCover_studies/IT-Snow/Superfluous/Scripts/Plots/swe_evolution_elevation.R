# The goal of this script is to make a SWE volume plot in which all elevation band 
# volumes are present, and also the total volume
# SWE volume evolution plot by elevation bands + total
rm(list = ls())
gc()

library(tidyr)
library(dplyr)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")

swe <- read.table("Datas/swe_evolution_elevation.dat", header = FALSE)

df <- data.frame(
  day  = seq_len(nrow(swe)),
  swe1 = swe$V1,
  swe2 = swe$V2,
  swe3 = swe$V3,
  swe4 = swe$V4,
  swe5 = swe$V5,
  swe6 = swe$V6,
  swe7 = swe$V7,
  swe8 = swe$V8,
  swe9 = swe$V9
)

# Total (summing nine bands)
df$total <- rowSums(df %>% select(starts_with("swe")), na.rm = TRUE)

long <- df %>%
  pivot_longer(
    cols = c(starts_with("swe"), total),
    names_to = "band",
    values_to = "volume"
  )

long$band <- factor(
  long$band,
  levels = c(paste0("swe", 1:9), "total"),
  labels = c(
    "0–500 m", "500–1000 m", "1000–1500 m",
    "1500–2000 m", "2000–2500 m", "2500–3000 m",
    "3000–3500 m", "3500–4000 m", "4000–5000 m",
    "Total"
  )
)


data0 <- as.Date("2010-09-01")
dates <- as.Date(paste0(2010:2025, "-09-01"))
dates_ind <- as.numeric(dates - data0) + 1

ggplot(long, aes(x = day, y = volume, color = band)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(
    breaks = dates_ind,
    labels = format(dates, "%d %b %Y"),
    expand = c(0, 0)
  ) +
  labs(
    title = "SWE evolution by elevation band",
    x = "Days",
    y = "SWE volume (Gm³)",
    color = "Band"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
