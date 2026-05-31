# The main goal of this script is to compare total SWE volume computation with 
# elevation-band wise, in order to look for procedure errors.
rm(list = ls())
gc()

library(tidyr)
library(dplyr)
library(ggplot2)

setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/IT-Snow")


# SWE elevation-wise analysis
swe <- read.table("Datas/swe_evolution_elevation.dat", header = FALSE)
df <- data.frame(
  day  = seq_len(nrow(swe)),
  swe1 = swe$V1, swe2 = swe$V2, swe3 = swe$V3,
  swe4 = swe$V4, swe5 = swe$V5, swe6 = swe$V6,
  swe7 = swe$V7, swe8 = swe$V8, swe9 = swe$V9
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

data0 <- as.Date("2010-09-01")
dates <- as.Date(paste0(2010:2025, "-09-01"))         # Creating date strings
dates_ind <- as.numeric(dates - data0) + 1            # Finding date indexes


# Plotting options (we will do a better job when it's all finished)
ggplot(df, aes(x = df$day, y = df$diffSWE)) + 
  scale_x_continuous(
    breaks = dates_ind,
    labels = format(dates, "%d %b %Y"), 
    expand = c(5e-3, 5e-3)
  ) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Difference between methods: 2011 to 2025", x = "Days", y = "SWE Gm^3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
