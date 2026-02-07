# The main goal of this script is to check whether we did a good job analyzing 
# SWE volume elevation-wise. We will import both dataset and plot the difference
# between total volume and cumulated one, just to have a feel of what we did.
rm(list = ls())
gc()

library(tidyr)
library(dplyr)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Po-Basin")


# SWE elevation-wise analysis
swe <- read.table("Datas/swe_evolution_elevation.dat", header = FALSE)
df <- data.frame(
  day  = seq_len(nrow(swe)),
  swe1 = swe$V1, swe2 = swe$V2, swe3 = swe$V3,
  swe4 = swe$V4, swe5 = swe$V5, swe6 = swe$V6
)
sweC <- rowSums(df %>% select(starts_with("swe")), na.rm = TRUE)


# Total SWE analysis
appo <- read.table("Datas/swe_evolution.dat", header = FALSE)
sweT <- appo$V1


# Plotting difference (here we would like to have 0)
diffSWE <- sweT - sweC
df <- data.frame(
  day = 1:length(diffSWE),
  diffSWE = diffSWE
)


# Plotting options (we will do a better job when it's all finished)
ggplot(df, aes(x = df$day, y = df$diffSWE)) + 
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Cumulated vs Total SWE volume", x = "Days", y = "SWE Gm^3") +
  theme_minimal()
