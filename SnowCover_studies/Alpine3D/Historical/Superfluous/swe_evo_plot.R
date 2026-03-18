# The main goal of this script is to make a swe evolution plot
rm(list = ls())
gc()

library(ncdf4)
library(ggplot2)
  
setwd("/home/filippo/Desktop/Codicini/Master_Thesis/SnowCover_studies/Alpine3D/Historical/")
  
swe <- read.table("Datas/swe_evolution.dat")
swe <- swe$V1
  
df <- data.frame(
  day = 1:length(swe),
  swe = swe
)

data0 <- as.Date("1961-09-01")
dates <- as.Date(paste0(1961:2023, "-09-01"))
dates_ind <- as.numeric(dates - data0) + 1
  

# Plotting options
dates_5y <- dates[seq(1, length(dates), by = 5)]
dates_ind_5y <- as.numeric(dates_5y - data0) + 1

ggplot(df, aes(x = day, y = swe)) + 
  scale_x_continuous(
    breaks = dates_ind_5y,
    labels = format(dates_5y, "%d %b %Y"),
    expand = c(5e-3, 5e-3)
  ) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Swiss SWE evolution: 1961 to 2023", x = "", y = "SWE [Gm³]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))