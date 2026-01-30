# The main goal of this script is to plot altitude frequency for 2011 season
rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")
df <- read.table("Datas/freqAltitude2011.dat", header = FALSE)


colnames(df) <- paste0("s", 1:9)

# Crea una variabile x (ad es. tempo/indice)
df$x <- seq_len(nrow(df))

# 2) Trasforma da wide a long (x, serie, y)
long <- df %>%
  pivot_longer(
    cols = starts_with("s"),
    names_to = "serie",
    values_to = "y"
  )
long$serie <- factor(long$serie, 
                     levels = rev(paste0("s", 1:9)))
# 3) Plot stacked area
ggplot(long, aes(x = x, y = y, fill = serie)) +
  geom_area(position = "stack", alpha = 0.9) +
  scale_fill_discrete(
    labels = c(
      "4000–4500 m",
      "3500–4000 m",
      "3000–3500 m",
      "2500–3000 m",
      "2000–2500 m",
      "1500–2000 m",
      "1000–1500 m",
      "500–1000 m",
      "0–500 m"
    )
  ) +
  theme_minimal() +
  labs(x = "Days", y = NULL, fill = NULL)
