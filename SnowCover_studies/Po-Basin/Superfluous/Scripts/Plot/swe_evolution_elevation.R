# The goal of this script is to make a SWE volume plot in which all elevation band 
# volumes are present, and also the total volume
# SWE volume evolution plot by elevation bands + total
rm(list = ls())
gc()

library(tidyr)
library(dplyr)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")

swe <- read.table("Datas/Volume_SWE/Evo_Ele_SWE/swe_evolution1992.dat", header = FALSE)
appo <- read.table("Datas/Volume_SWE/Evo_SWE/swe_evolution1992.dat", header = FALSE)
appo <- appo$V1

df <- data.frame(
  day  = seq_len(367),
  swe1 = c(swe$V1, array(NA_real_, dim = c(94))),
  swe2 = c(swe$V2, array(NA_real_, dim = c(94))),
  swe3 = c(swe$V3, array(NA_real_, dim = c(94))),
  swe4 = c(swe$V4, array(NA_real_, dim = c(94))),
  swe5 = c(swe$V5, array(NA_real_, dim = c(94))),
  swe6 = c(swe$V6, array(NA_real_, dim = c(94))) 
)

# Total (summing six elevation bands)
swe_cols <- df %>% select(starts_with("swe"))
df$total <- ifelse(rowSums(!is.na(swe_cols)) == 0, NA_real_, rowSums(swe_cols, na.rm = TRUE))

long <- df %>%
  pivot_longer(
    cols = c(starts_with("swe"), total),
    names_to = "band",
    values_to = "volume"
  )

long$band <- factor(
  long$band,
  levels = c(paste0("swe", 1:6), "total"),
  labels = c(
    "0–500 m", "500–1000 m", "1000–1500 m",
    "1500–2000 m", "2000–2500 m", "2500–4500 m",
    "Total"
  )
)

pal_bands <- c(
  "0–500 m"     = "#1b9e77",
  "500–1000 m"  = "#66a61e",
  "1000–1500 m" = "#a6d854",
  "1500–2000 m" = "#ffd92f",
  "2000–2500 m" = "#e78ac3",
  "2500–4500 m" = "#7570b3",
  "Total"       = "#000000"
)


data0 <- as.Date("1991-10-03")
dates <- as.Date(paste0(1991:1992, "-10-03"))
dates_ind <- as.numeric(dates - data0) + 1

ggplot(long, aes(x = day, y = volume, color = band)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = pal_bands) +
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
